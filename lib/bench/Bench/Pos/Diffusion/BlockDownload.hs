-- A program which runs two diffusion layers over a TCP transport, and has
-- one of them request blocks from the other.
-- Criterion benchmarks are done, which of course benchmarks the client and
-- server as one. We can work on benchmarking them in separate processes
-- later if need be.
-- Currently only the batched block requests are wired up. The streaming
-- definition is not yet available.
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bench.Pos.Diffusion.BlockDownload where

import           Universum

import           Control.DeepSeq (force)
import           Control.Monad.IO.Class (liftIO)
import qualified Criterion
import qualified Criterion.Main as Criterion
import qualified Criterion.Main.Options as Criterion
import qualified Data.ByteString.Lazy as LBS
import           Data.Semigroup ((<>))
import qualified Options.Applicative as Opt (execParser)

import           Data.List.NonEmpty (NonEmpty ((:|)))
import           Data.Time.Units (Microsecond)
import qualified Network.Broadcast.OutboundQueue as OQ
import qualified Network.Broadcast.OutboundQueue.Types as OQ
import           Network.Transport (Transport)
import qualified Network.Transport.TCP as TCP
import           Node (NodeId)
import qualified Node
import           Pipes (each)

import           Pos.Binary (serialize, serialize')
import           Pos.Core (Block, BlockHeader, BlockVersion (..), HeaderHash)
import qualified Pos.Core as Core (getBlockHeader)
import           Pos.Core.ProtocolConstants (ProtocolConstants (..))
import           Pos.Crypto (ProtocolMagic (..))
import           Pos.Crypto.Hashing (Hash, unsafeMkAbstractHash)
import           Pos.DB.Class (Serialized (..), SerializedBlock)
import           Pos.Diffusion.Full (FullDiffusionConfiguration (..),
                     FullDiffusionInternals (..),
                     RunFullDiffusionInternals (..),
                     diffusionLayerFullExposeInternals)
import qualified Pos.Infra.Diffusion.Transport.TCP as Diffusion
                     (bracketTransportTCP)
import           Pos.Infra.Diffusion.Types as Diffusion (Diffusion (..))
import qualified Pos.Infra.Network.Policy as Policy
import           Pos.Infra.Network.Types (Bucket (..))
import           Pos.Infra.Reporting.Health.Types (HealthStatus (..))
import           Pos.Logic.Pure (pureLogic)
import           Pos.Logic.Types as Logic (Logic (..))

import           Pos.Core.Chrono (NewestFirst (..), OldestFirst (..))
import           Pos.Util.Trace (noTrace, wlogTrace)
import           Test.Pos.Block.Arbitrary.Generate (generateMainBlock)

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

someOtherHash :: forall a . Hash a
someOtherHash = unsafeMkAbstractHash (LBS.pack [0x00])

-- | Grab a TCP transport at 127.0.0.1:0 with 15s timeout.
-- Uses the stock parameters from 'Pos.Diffusion.Transport.bracketTransportTCP'
-- which are also used in production (fair QDisc etc.).
withTransport :: (Transport -> IO t) -> IO t
withTransport k =
    Diffusion.bracketTransportTCP noTrace connectionTimeout tcpAddr k
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
    :: IORef [Block] -- ^ For streaming, so we can control how many are given.
    -> Block
    -> NonEmpty HeaderHash
    -> NonEmpty BlockHeader
    -> Logic IO
serverLogic streamIORef arbitraryBlock arbitraryHashes arbitraryHeaders = pureLogic
    { getSerializedBlock = const (pure (Just $ serializedBlock arbitraryBlock))
    , getBlockHeader = const (pure (Just (Core.getBlockHeader arbitraryBlock)))
    , getHashesRange = \_ _ _ -> pure (Right (OldestFirst arbitraryHashes))
    , getBlockHeaders = \_ _ _ -> pure (Right (NewestFirst arbitraryHeaders))
    , getTip = pure arbitraryBlock
    , getTipHeader = pure (Core.getBlockHeader arbitraryBlock)
    , Logic.streamBlocks = \_ -> do
          bs <-  readIORef streamIORef
          each $ map serializedBlock bs
    }

serializedBlock :: Block -> SerializedBlock
serializedBlock = Serialized . serialize'

-- Modify a pure logic layer so that the LCA computation (suffix not in the
-- chain) always gives the entire thing. This makes the batch block requester
-- always ask for the entire chain (and not throw an exception).
clientLogic :: Logic IO
clientLogic = pureLogic
    { getLcaMainChain = \headers -> pure (NewestFirst [], headers)
    }

withServer :: Transport -> Logic IO -> (NodeId -> IO t) -> IO t
withServer transport logic k = do
    -- Morally, the server shouldn't need an outbound queue, but we have to
    -- give one.
    oq <- liftIO $ OQ.new
                 (wlogTrace ("server" <> "outboundqueue"))
                 Policy.defaultEnqueuePolicyRelay
                 --Policy.defaultDequeuePolicyRelay
                 (const (OQ.Dequeue OQ.NoRateLimiting (OQ.MaxInFlight maxBound)))
                 Policy.defaultFailurePolicyAuxx -- because its timeout is 0
                 (const (OQ.BucketSizeUnlimited))
                 (OQ.UnknownNodeType (const OQ.NodeRelay))
    (_, runInternals) <- diffusionLayerFullExposeInternals
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
        , fdcStreamWindow = 65536
        , fdcTrace = wlogTrace ("server" <> "diffusion")
        }

-- Like 'withServer' but we must set up the outbound queue so that it will
-- contact the server.
withClient
    :: Transport
    -> Logic IO
    -> NodeId
    -> (Diffusion IO -> IO t)
    -> IO t
withClient transport logic serverAddress@(Node.NodeId _) k = do
    -- Morally, the server shouldn't need an outbound queue, but we have to
    -- give one.
    oq <- OQ.new
                 (wlogTrace ("client" <> "outboundqueue"))
                 Policy.defaultEnqueuePolicyRelay
                 --Policy.defaultDequeuePolicyRelay
                 (const (OQ.Dequeue OQ.NoRateLimiting (OQ.MaxInFlight maxBound)))
                 Policy.defaultFailurePolicyAuxx -- because its timeout is 0
                 (const (OQ.BucketSizeUnlimited))
                 (OQ.UnknownNodeType (const OQ.NodeRelay))
    _ <- OQ.updatePeersBucket oq BucketStatic $ \_ ->
        OQ.simplePeers [(OQ.NodeRelay, serverAddress)]
    (diffusion, runInternals) <- diffusionLayerFullExposeInternals
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
        , fdcStreamWindow = 65536
        , fdcTrace = wlogTrace ("client" <> "diffusion")
        }


-- Final parameter is the number of batches to do. Total blocks downloaded is
-- this number multiplies by 2200 (the production value of the cooly-named
-- 'recoveryHeadersMessage' parameter).
blockDownloadBatch :: NodeId -> Diffusion IO -> (HeaderHash, [HeaderHash]) -> Int -> IO ()
blockDownloadBatch serverAddress client ~(blockHeader, checkpoints) batches =  do
    -- We have to manually batch, because that's what happens in cardano-sl
    -- production: the retrieval queue logic (currently outside of the diffusion
    -- layer) batches in groups of 2200.
    -- We won't do any work in-between, so we'll get better performance than
    -- we would see in production (if streaming is faster, our results will be
    -- a lower bound on the real speedup).
    forM_ [1..batches] $ \_ ->
        getBlocks client serverAddress blockHeader checkpoints

-- Final parameter, like for 'blockDownloadBatch', is the number of batches to
-- do. Here, in streaming, we multiply by 2200 to make a fair comparison with
-- 'blockDownloadBatch', which will do 2200 at a time.
blockDownloadStream :: NodeId -> (Int -> IO ()) -> Diffusion IO -> (HeaderHash, [HeaderHash]) -> Int -> IO ()
blockDownloadStream serverAddress setStreamIORef client ~(blockHeader, checkpoints) batches = do
    setStreamIORef numBlocks
    _ <- Diffusion.streamBlocks client serverAddress blockHeader checkpoints writeCallback
    return ()
  where
    numBlocks = batches * 2200

    writeCallback !_ = return ()

blockDownloadBenchmarks :: NodeId -> (Int -> IO ()) -> Diffusion IO -> [Criterion.Benchmark]
blockDownloadBenchmarks serverAddress setStreamIORef client =
    [ Criterion.bgroup "batch"  $ blockDownloadBatchBenchmarks serverAddress client
    , Criterion.bgroup "stream" $ blockDownloadStreamBenchmarks serverAddress setStreamIORef client
    ]

blockDownloadBatchBenchmarks :: NodeId -> Diffusion IO -> [Criterion.Benchmark]
blockDownloadBatchBenchmarks serverAddress client =
    [ Criterion.env batchParams $ \batchParams' -> Criterion.bgroup "download"
          [ Criterion.bench "1" $ Criterion.whnfIO (blockDownloadBatch serverAddress client batchParams' 1)
          , Criterion.bench "2" $ Criterion.whnfIO (blockDownloadBatch serverAddress client batchParams' 2)
          , Criterion.bench "4" $ Criterion.whnfIO (blockDownloadBatch serverAddress client batchParams' 4)
          ]
    ]
  where
    batchParams :: IO (HeaderHash, [HeaderHash])
    -- Checkpoint must not be the same as the tip, or else the batch downloader
    -- will take a different path and request only one block.
    batchParams = pure (someHash, [someOtherHash])

blockDownloadStreamBenchmarks :: NodeId -> (Int -> IO ()) -> Diffusion IO -> [Criterion.Benchmark]
blockDownloadStreamBenchmarks serverAddress setStreamIORef client =
    [ Criterion.env streamParams $ \batchParams -> Criterion.bgroup "download"
          [ Criterion.bench "1" $ Criterion.whnfIO (blockDownloadStream serverAddress setStreamIORef client batchParams 1)
          , Criterion.bench "2" $ Criterion.whnfIO (blockDownloadStream serverAddress setStreamIORef client batchParams 2)
          , Criterion.bench "4" $ Criterion.whnfIO (blockDownloadStream serverAddress setStreamIORef client batchParams 4)
          ]
    ]

  where
    streamParams :: IO (HeaderHash, [HeaderHash])
    streamParams = pure (someHash, [someHash])

runBlockDownloadBenchmark :: Criterion.Mode -> NodeId -> (Int -> IO ()) -> Diffusion IO -> IO ()
runBlockDownloadBenchmark mode serverAddress setStreamIORef client =
    Criterion.runMode mode $ blockDownloadBenchmarks serverAddress setStreamIORef client

runBenchmark :: IO ()
runBenchmark = do
    {-
    Wlog.setupLogging Nothing $ (Wlog.defaultConfig "arbitrary_logger_name")
        { Wlog._lcTree = Wlog.LoggerTree mempty [] (Just Wlog.allSeverities)
        }
    -}
    -- Parse criterion arguments before setting anything up. Wouldn't want to
    -- bring up a transport if the arguments don't parse.
    criterionMode <- Opt.execParser (Criterion.describe Criterion.defaultConfig)
    putStrLn ("Generating and forcing the necessary blockchain data ..." :: String)
    streamIORef <- newIORef []
    let seed = 0
        size = 4
        !arbitraryBlock = force $ Right (generateMainBlock protocolMagic protocolConstants seed size)
        !arbitraryHashes = force $ someHash :| replicate 2199 someHash
        !arbitraryHeader = force $ Core.getBlockHeader arbitraryBlock
        !arbitraryHeaders = force $ arbitraryHeader :| replicate 2199 arbitraryHeader
        blockSize = LBS.length $ serialize arbitraryBlock
        setStreamIORef = \n -> writeIORef streamIORef (replicate n arbitraryBlock)
    putStrLn $ "Using block of size " ++ show blockSize ++ " bytes"
    putStrLn ("Bringing up client and server infrastructure ..." :: String)
    withTransport $ \transport ->
        withServer transport (serverLogic streamIORef arbitraryBlock arbitraryHashes arbitraryHeaders) $ \serverAddress ->
        -- client needs the serverAddress so that it can put it into its
        -- outbound queue.
        withClient transport clientLogic serverAddress $
            liftIO . runBlockDownloadBenchmark criterionMode serverAddress setStreamIORef
