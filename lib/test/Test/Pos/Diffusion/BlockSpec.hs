
module Test.Pos.Diffusion.BlockSpec
    ( spec
    ) where

import           Universum


import           Control.Concurrent.STM (readTBQueue)
import           Control.DeepSeq (NFData, force)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as LBS
import           Data.Semigroup ((<>))
import           Test.Hspec (Spec, describe, it, shouldBe)

import           Data.Bits
import           Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import           Data.Time.Units (Microsecond)
import qualified Network.Broadcast.OutboundQueue as OQ
import qualified Network.Broadcast.OutboundQueue.Types as OQ
import           Network.Transport (Transport)
import qualified Network.Transport.TCP as TCP
import           Node (NodeId)
import qualified Node
import           Pipes (each)

import           Pos.Arbitrary.Block.Generate (generateMainBlock)
import           Pos.Core (Block, BlockHeader, BlockVersion (..), HeaderHash, blockHeaderHash)
import qualified Pos.Core as Core (getBlockHeader)
import           Pos.Core.ProtocolConstants (ProtocolConstants (..))
import           Pos.Crypto (ProtocolMagic (..))
import           Pos.Crypto.Hashing (Hash, unsafeMkAbstractHash)
import           Pos.Diffusion.Full (FullDiffusionConfiguration (..), FullDiffusionInternals (..),
                                     RunFullDiffusionInternals (..),
                                     diffusionLayerFullExposeInternals)
import qualified Pos.Diffusion.Transport.TCP as Diffusion (bracketTransportTCP)
import           Pos.Diffusion.Types as Diffusion (Diffusion (..), StreamEntry (..))
import           Pos.Logic.Pure (pureLogic)
import           Pos.Logic.Types as Logic (Logic (..))
import qualified Pos.Network.Policy as Policy
import           Pos.Network.Types (Bucket (..))
import           Pos.Reporting.Health.Types (HealthStatus (..))

import           Pos.Util.Chrono (NewestFirst (..), OldestFirst (..))
import           Pos.Util.Trace (noTrace, wlogTrace)

-- HLint warning disabled since I ran into https://ghc.haskell.org/trac/ghc/ticket/13106
-- when trying to resolve it.
{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

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

someHash' :: forall a . Int -> [Word8] -> Hash a
someHash' 0 r = unsafeMkAbstractHash (LBS.pack r)
someHash' x r =
    let v = fromIntegral $ x .&. 0xff
        x' = x `shiftR` 8 in
    someHash' x' (v:r)

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
    { getBlock = const (pure (Just arbitraryBlock))
    , getBlockHeader = const (pure (Just (Core.getBlockHeader arbitraryBlock)))
    , getHashesRange = \_ _ _ -> pure (Right (OldestFirst arbitraryHashes))
    , getBlockHeaders = \_ _ _ -> pure (Right (NewestFirst arbitraryHeaders))
    , getTip = pure arbitraryBlock
    , getTipHeader = pure (Core.getBlockHeader arbitraryBlock)
    , Logic.streamBlocks = \_ -> do
          blocks <- readIORef streamIORef
          each blocks
    }

-- Modify a pure logic layer so that the LCA computation (suffix not in the
-- chain) always gives the entire thing. This makes the batch block requester
-- always ask for the entire chain (and not throw an exception).
clientLogic :: Logic IO
clientLogic = pureLogic
    { getLcaMainChain = \headers -> pure headers
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
        , fdcStreamWindow = 2048
        , fdcTrace = wlogTrace ("server" <> "diffusion")
        }

-- Like 'withServer' but we must set up the outbound queue so that it will
-- contact the server.
withClient
    :: Word32
    -> Transport
    -> Logic IO
    -> NodeId
    -> (Diffusion IO -> IO t)
    -> IO t
withClient streamWindow transport logic serverAddress@(Node.NodeId _) k = do
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
        , fdcStreamWindow = streamWindow
        , fdcTrace = wlogTrace ("client" <> "diffusion")
        }


-- Final parameter is the number of batches to do. Total blocks downloaded is
-- this number multiplies by 2200 (the production value of the cooly-named
-- 'recoveryHeadersMessage' parameter).
blockDownloadBatch :: NodeId -> (HeaderHash, [HeaderHash]) -> Diffusion IO -> IO ()
blockDownloadBatch serverAddress ~(blockHeader, checkpoints) client =  do
    !_ <- getBlocks client serverAddress blockHeader checkpoints
    return ()

blockDownloadStream :: NodeId -> IORef Bool -> IORef [Block] -> (Int -> IO ()) -> (HeaderHash, [HeaderHash]) -> Diffusion IO-> IO ()
blockDownloadStream serverAddress resultIORef streamIORef setStreamIORef ~(blockHeader, checkpoints) client = do
    setStreamIORef 1
    _ <- Diffusion.streamBlocks client serverAddress blockHeader checkpoints (loop (0::Word32) [])
    return ()
  where
    loop n recvBlocks (streamWindow, wqgM, blockChan) = do
        streamEntry <- atomically $ readTBQueue blockChan
        case streamEntry of
          StreamEnd         -> do
              expectedBlocks <- readIORef streamIORef
              writeIORef resultIORef $ expectedBlocks == reverse recvBlocks
              return ()
          StreamBlock !b -> do
              loop n (b : recvBlocks) (streamWindow, wqgM, blockChan)


-- It's surprisingly cumbersome to give a non-orphan 'NFData' instance on
-- 'BlockHeader', since we have that 'Blockchain' typeclass with a bunch of
-- data families. 'BHeaderHash GenesisBlockchain', for instance, must have
-- an 'NFData' instance, but in that module we don't yet know that this is
-- in fact 'HeaderHash' ~ 'Crypto.Digest Blake2b_256'.
-- Anyway, there's a whole saga of pain caused by that silly abstraction.
instance NFData BlockHeader

-- Generate a list of n+1 blocks
generateBlocks :: Int -> NonEmpty Block
generateBlocks blocks =
  let root = doGenerateBlock 0 in
  root :| (doGenerateBlocks blocks)
  where
    doGenerateBlock :: Int -> Block
    doGenerateBlock seed =
        let size = 4 in
        force $ Right (generateMainBlock protocolMagic protocolConstants seed size)

    doGenerateBlocks :: Int -> [Block]
    doGenerateBlocks 0 = []
    doGenerateBlocks x = do
        [doGenerateBlock x] ++ (doGenerateBlocks (x-1))

streamSimple :: Word32 -> Int -> IO Bool
streamSimple streamWindow blocks = do
    streamIORef <- newIORef []
    resultIORef <- newIORef False
    let arbitraryBlocks = generateBlocks (blocks - 1)
        arbitraryHeaders = NE.map Core.getBlockHeader arbitraryBlocks
        arbitraryHashes = NE.map blockHeaderHash arbitraryHeaders
        !arbitraryBlock = NE.head arbitraryBlocks
        tipHash = NE.head arbitraryHashes
        checkpoints = [tipHash]
        setStreamIORef = \_ -> writeIORef streamIORef $ NE.tail arbitraryBlocks
    withTransport $ \transport ->
        withServer transport (serverLogic streamIORef arbitraryBlock arbitraryHashes arbitraryHeaders) $ \serverAddress ->
        withClient streamWindow transport clientLogic serverAddress $
            liftIO . blockDownloadStream serverAddress resultIORef streamIORef setStreamIORef
                (tipHash, checkpoints)
    readIORef resultIORef

batchSimple :: Int -> IO Bool
batchSimple blocks = do
    streamIORef <- newIORef []
    let arbitraryBlocks = generateBlocks (blocks - 1)
        arbitraryHeaders = NE.map Core.getBlockHeader arbitraryBlocks
        arbitraryHashes = NE.map blockHeaderHash arbitraryHeaders
        arbitraryBlock = NE.head arbitraryBlocks
        !checkPoints = if blocks == 1 then [someHash]
                                      else [someHash' (blocks + 1) []]
    withTransport $ \transport ->
        withServer transport (serverLogic streamIORef arbitraryBlock arbitraryHashes arbitraryHeaders) $ \serverAddress ->
        withClient 2048 transport clientLogic serverAddress $
            liftIO . blockDownloadBatch serverAddress (someHash, checkPoints)
    return True

spec :: Spec
spec = describe "Blockdownload" $ do
    it "Stream 4 blocks" $ do
        r <- streamSimple 2048 4
        r `shouldBe` True
    it "Stream 128 blocks" $ do
        r <- streamSimple 2048 128
        r `shouldBe` True
    it "Stream 4096 blocks" $ do
        r <- streamSimple 128 4096
        r `shouldBe` True
    it "Streaming dislabed by client" $ do
        r <- streamSimple 0 4
        r `shouldBe` False
    it "Batch, single block" $ do
        r <- batchSimple 1
        r `shouldBe` True
    it "Batch of blocks" $ do
        r <- batchSimple 2200
        r `shouldBe` True

