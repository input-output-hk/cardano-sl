-- A program which runs two diffusion layers over a TCP transport, and has
-- one of them request blocks from the other.
-- Criterion benchmarks are done, which of course benchmarks the client and
-- server as one. We can work on benchmarking them in separate processes
-- later if need be.
-- Currently only the batched block requests are wired up. The streaming
-- definition is not yet available.

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

import           Control.DeepSeq (NFData, force)
import           Control.Monad (forM_)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.IO.Class (liftIO)
import qualified Criterion as Criterion
import qualified Criterion.Main as Criterion
import qualified Criterion.Main.Options as Criterion
import qualified Data.ByteString.Lazy as LBS
import qualified Options.Applicative as Opt (execParser)

import           Data.List.NonEmpty (NonEmpty ((:|)))
import           Data.Time.Units (Microsecond)
import qualified Network.Broadcast.OutboundQueue as OQ
import qualified Network.Broadcast.OutboundQueue.Types as OQ
import           Network.Transport.Abstract (Transport)
import qualified Network.Transport.TCP as TCP
import           Node (NodeId)
import qualified Node
import           Mockable.Production (Production, runProduction)
import           System.Wlog (usingLoggerName)

import           Pos.Arbitrary.Block.Generate (generateMainBlock)
import           Pos.Binary (serialize)
import           Pos.Core (BlockVersion (..), Block, HeaderHash, BlockHeader)
import qualified Pos.Core as Core (getBlockHeader)
import           Pos.Core.ProtocolConstants (ProtocolConstants (..))
import           Pos.Crypto (ProtocolMagic (..))
import           Pos.Crypto.Hashing (Hash, unsafeMkAbstractHash)
import           Pos.DHT.Real (KademliaParams (..))
import           Pos.Network.Types (NetworkConfig (..), Bucket (..))
import qualified Pos.Network.Policy as Policy
import           Pos.Diffusion.Full (FullDiffusionConfiguration (..),
                                     FullDiffusionInternals (..),
                                     RunFullDiffusionInternals (..),
                                     diffusionLayerFullExposeInternals)
import qualified Pos.Diffusion.Transport.TCP as Diffusion (bracketTransportTCP)
import           Pos.Diffusion.Types (Diffusion (..), DiffusionLayer (..))
import           Pos.Logic.Types (Logic (..), LogicLayer (..))
import           Pos.Logic.Pure (pureLogic)
import           Pos.Reporting.Health.Types (HealthStatus (..))

import           Pos.Util.Chrono (NewestFirst (..), OldestFirst (..))
import           Pos.Util.TimeWarp (addressToNodeId)

-- TODO
--
--   We do not set up a subscription connection. This _probably_ means that
--   the batching performance will be somewhat skewed unfavourably, because
--   network-transport-tcp may be setting up more TCP connections that it
--   would be in the case of an end-user syncing: in that case, the
--   subscription logical connection keeps the socket up throughout.
--   However, I believe that for relay<->relay syncing, in which there is
--   no subscription connection, we would see this problem.

protocolMagic :: ProtocolMagic
protocolMagic = ProtocolMagic 0

protocolConstants :: ProtocolConstants
protocolConstants = ProtocolConstants
    { pcK = 2
    , pcVssMinTTL = minBound
    , pcVssMaxTTL = maxBound
    }

blockVersion :: BlockVersion
blockVersion = BlockVersion
    { bvMajor = 0
    , bvMinor = 0
    , bvAlt   = 0
    }

someHash :: forall a . Hash a
someHash = unsafeMkAbstractHash LBS.empty

-- | Grab a TCP transport at 127.0.0.1:0 with 15s timeout.
-- Uses the stock parameters from 'Pos.Diffusion.Transport.bracketTransportTCP'
-- which are also used in production (fair QDisc etc.).
withTransport :: (Transport Production -> Production t) -> Production t
withTransport k = usingLoggerName "" $
    -- the 'lift' bring it into 'LoggerNameBox Production', which we're
    -- working in here, but which is discharged by that
    -- 'usingLoggerName ""'
    Diffusion.bracketTransportTCP connectionTimeout tcpAddr (lift . k)
  where
    connectionTimeout :: Microsecond
    connectionTimeout = 15000000
    tcpAddr :: TCP.TCPAddr
    tcpAddr = TCP.Addressable $ TCP.TCPAddrInfo
        { TCP.tcpBindHost = "127.0.0.1"
        , TCP.tcpBindPort = "0"
        , TCP.tcpExternalAddress = (,) "127.0.0.1"
        }

serverLogic
    :: Block
    -> NonEmpty HeaderHash
    -> NonEmpty BlockHeader
    -> Logic Production
serverLogic arbitraryBlock arbitraryHashes arbitraryHeaders = pureLogic
    { getBlock = const (pure (Just arbitraryBlock))
    , getBlockHeader = const (pure (Just (Core.getBlockHeader arbitraryBlock)))
    , getHashesRange = \_ _ _ -> pure (Right (OldestFirst arbitraryHashes))
    , getBlockHeaders = \_ _ _ -> pure (Right (NewestFirst arbitraryHeaders))
    , getTip = pure arbitraryBlock
    , getTipHeader = pure (Core.getBlockHeader arbitraryBlock)
    }

-- Modify a pure logic layer so that the LCA computation (suffix not in the
-- chain) always gives the entire thing. This makes the batch block requester
-- always ask for the entire chain (and not throw an exception).
clientLogic :: Logic Production
clientLogic = pureLogic
    { getLcaMainChain = \headers -> pure headers
    }

withServer :: Transport Production -> Logic Production -> (NodeId -> Production t) -> Production t
withServer transport logic k = do
    -- Morally, the server shouldn't need an outbound queue, but we have to
    -- give one.
    oq <- liftIO $ OQ.new "server"
                 Policy.defaultEnqueuePolicyRelay
                 Policy.defaultDequeuePolicyRelay
                 Policy.defaultFailurePolicyAuxx -- because its timeout is 0
                 (const (OQ.BucketSizeMax 0))
                 (OQ.UnknownNodeType (const OQ.NodeRelay))
    (diffusion, runInternals) <- diffusionLayerFullExposeInternals
        runProduction
        fdconf
        transport
        oq
        3000 -- default port
        Nothing -- subscription worker
        Nothing -- policy for subscribers
        Nothing -- Use kademlia? No
        (pure (HSHealthy "")) -- Don't care about health status
        Nothing -- EKG metrics, don't care
        logic
    runFullDiffusionInternals runInternals $ \internals -> k (Node.nodeId (fdiNode internals))
  where
    fdconf = FullDiffusionConfiguration
        { fdcProtocolMagic = protocolMagic
        , fdcProtocolConstants = protocolConstants
        -- Just like in production.
        , fdcRecoveryHeadersMessage = 2200
        , fdcLastKnownBlockVersion = blockVersion
        , fdcConvEstablishTimeout = 15000000 -- us
        }

-- Like 'withServer' but we must set up the outbound queue so that it will
-- contact the server.
withClient
    :: Transport Production
    -> Logic Production
    -> NodeId
    -> (Diffusion Production -> Production t)
    -> Production t
withClient transport logic serverAddress k = do
    -- Morally, the server shouldn't need an outbound queue, but we have to
    -- give one.
    oq <- liftIO $ OQ.new "server"
                 Policy.defaultEnqueuePolicyRelay
                 Policy.defaultDequeuePolicyRelay
                 Policy.defaultFailurePolicyAuxx -- because its timeout is 0
                 (const (OQ.BucketSizeMax 0))
                 (OQ.UnknownNodeType (const OQ.NodeRelay))
    _ <- liftIO $ OQ.updatePeersBucket oq BucketStatic $ \_ ->
        OQ.simplePeers [(OQ.NodeRelay, serverAddress)]
    (diffusion, runInternals) <- diffusionLayerFullExposeInternals
        runProduction
        fdconf
        transport
        oq
        3000 -- default port
        Nothing -- subscription worker
        Nothing -- policy for subscribers
        Nothing -- Use kademlia? No
        (pure (HSHealthy "")) -- Don't care about health status
        Nothing -- EKG metrics, don't care
        logic
    runFullDiffusionInternals runInternals $ \_ -> k diffusion
  where
    fdconf = FullDiffusionConfiguration
        { fdcProtocolMagic = protocolMagic
        , fdcProtocolConstants = protocolConstants
        -- Just like in production.
        , fdcRecoveryHeadersMessage = 2200
        , fdcLastKnownBlockVersion = blockVersion
        , fdcConvEstablishTimeout = 15000000 -- us
        }


-- Final parameter is the number of batches to do. Total blocks downloaded is
-- this number multiplies by 2200 (the production value of the cooly-named
-- 'recoveryHeadersMessage' parameter).
blockDownloadBatch :: NodeId -> Diffusion Production -> (HeaderHash, [HeaderHash]) -> Int -> IO ()
blockDownloadBatch serverAddress client ~(blockHeader, checkpoints) batches = runProduction $ do
    -- We have to manually batch, because that's what happens in cardano-sl
    -- production: the retrieval queue logic (currently outside of the diffusion
    -- layer) batches in groups of 2200.
    -- We won't do any work in-between, so we'll get better performance than
    -- we would see in production (if streaming is faster, our results will be
    -- a lower bound on the real speedup).
    forM_ [1..batches] $ \_ ->
        getBlocks client serverAddress blockHeader checkpoints
    pure ()

-- Final parameter, like for 'blockDownloadBatch', is the number of batches to
-- do. Here, in streaming, we multiply by 2200 to make a fair comparison with
-- 'blockDownloadBatch', which will do 2200 at a time.
blockDownloadStream :: NodeId -> Diffusion Production -> (HeaderHash, [HeaderHash]) -> Int -> IO ()
blockDownloadStream serverAddress client ~(blockHeader, checkpoints) batches = runProduction $
    pure () -- streamBlocks client serverAddress blockHeader checkpoints
  where
    numBlocks = batches * 2200

blockDownloadBenchmarks :: NodeId -> Diffusion Production -> [Criterion.Benchmark]
blockDownloadBenchmarks serverAddress client =
    [ Criterion.bgroup "batch"  $ blockDownloadBatchBenchmarks serverAddress client
    , Criterion.bgroup "stream" $ blockDownloadStreamBenchmarks serverAddress client
    ]

blockDownloadBatchBenchmarks :: NodeId -> Diffusion Production -> [Criterion.Benchmark]
blockDownloadBatchBenchmarks serverAddress client =
    [ Criterion.env batchParams $ \batchParams -> Criterion.bgroup "download"
          [ Criterion.bench "1" $ Criterion.whnfIO (blockDownloadBatch serverAddress client batchParams 1)
          , Criterion.bench "10" $ Criterion.whnfIO (blockDownloadBatch serverAddress client batchParams 10)
          , Criterion.bench "100" $ Criterion.whnfIO (blockDownloadBatch serverAddress client batchParams 100)
          , Criterion.bench "1000" $ Criterion.whnfIO (blockDownloadBatch serverAddress client batchParams 1000)
          ]
    ]
  where
    batchParams :: IO (HeaderHash, [HeaderHash])
    batchParams = pure (someHash, [])

blockDownloadStreamBenchmarks :: NodeId -> Diffusion Production -> [Criterion.Benchmark]
blockDownloadStreamBenchmarks serverAddress client =
    [ Criterion.env streamParams $ \batchParams -> Criterion.bgroup "download"
          [ Criterion.bench "1" $ Criterion.whnfIO (blockDownloadStream serverAddress client batchParams 1)
          , Criterion.bench "10" $ Criterion.whnfIO (blockDownloadStream serverAddress client batchParams 10)
          , Criterion.bench "100" $ Criterion.whnfIO (blockDownloadStream serverAddress client batchParams 100)
          , Criterion.bench "1000" $ Criterion.whnfIO (blockDownloadStream serverAddress client batchParams 1000)
          ]
    ]

  where
    streamParams :: IO (HeaderHash, [HeaderHash])
    streamParams = pure (someHash, [])

runBlockDownloadBenchmark :: Criterion.Mode -> NodeId -> Diffusion Production -> IO ()
runBlockDownloadBenchmark mode serverAddress client =
    Criterion.runMode mode $ blockDownloadBenchmarks serverAddress client

-- It's surprisingly cumbersome to give a non-orphan 'NFData' instance on
-- 'BlockHeader', since we have that 'Blockchain' typeclass with a bunch of
-- data families. 'BHeaderHash GenesisBlockchain', for instance, must have
-- an 'NFData' instance, but in that module we don't yet know that this is
-- in fact 'HeaderHash' ~ 'Crypto.Digest Blake2b_256'.
-- Anyway, there's a whole saga of pain caused by that silly abstraction.
instance NFData BlockHeader

main :: IO ()
main = do
    -- Parse criterion arguments before setting anything up. Wouldn't want to
    -- bring up a transport if the arguments don't parse.
    criterionMode <- Opt.execParser (Criterion.describe Criterion.defaultConfig)
    putStrLn "Generating and forcing the necessary blockchain data ..."
    let seed = 0
        size = 4
        !arbitraryBlock = force $ Right (generateMainBlock protocolMagic protocolConstants seed size)
        !arbitraryHashes = force $ someHash :| replicate 2199 someHash
        !arbitraryHeader = force $ Core.getBlockHeader arbitraryBlock
        !arbitraryHeaders = force $ arbitraryHeader :| replicate 2199 arbitraryHeader
        !arbitraryHeaderHash = force someHash
        blockSize = LBS.length $ serialize arbitraryBlock
    putStrLn $ "Using block of size " ++ show blockSize ++ " bytes"
    putStrLn "Bringing up client and server infrastructure ..."
    runProduction $ withTransport $ \transport ->
        withServer transport (serverLogic arbitraryBlock arbitraryHashes arbitraryHeaders) $ \serverAddress ->
        -- client needs the serverAddress so that it can put it into its
        -- outbound queue.
        withClient transport clientLogic serverAddress $
            liftIO . runBlockDownloadBenchmark criterionMode serverAddress
