{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes   #-}

module Test.Pos.Diffusion.BlockSpec
    ( spec
    ) where

import           Universum


import           Control.Concurrent.STM (readTBQueue)
--import           Control.DeepSeq (NFData, force)
import           Control.DeepSeq (force)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as LBS
import           Data.Semigroup ((<>))
import           Test.Hspec (Spec, describe, it, runIO, shouldBe)
import           Test.QuickCheck (arbitrary, generate)

import           Data.Bits
import           Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import qualified Network.Broadcast.OutboundQueue as OQ
import qualified Network.Broadcast.OutboundQueue.Types as OQ
import           Network.Transport (Transport, closeTransport)
import qualified Network.Transport.InMemory as InMemory
import           Node (NodeId)
import qualified Node
import           Pipes (each)

import           Pos.Binary.Class (serialize')
import           Pos.Core (Block, BlockHeader, BlockVersion (..), HeaderHash, blockHeaderHash)
import qualified Pos.Core as Core (getBlockHeader)
import           Pos.Core.ProtocolConstants (ProtocolConstants (..))
import           Pos.Crypto (ProtocolMagic (..), RequiresNetworkMagic (..))
import           Pos.Crypto.Hashing (Hash, unsafeMkAbstractHash)
import           Pos.DB.Class (Serialized (..), SerializedBlock)
import           Pos.Diffusion.Full (FullDiffusionConfiguration (..), FullDiffusionInternals (..),
                                     RunFullDiffusionInternals (..),
                                     diffusionLayerFullExposeInternals)
import           Pos.Infra.Diffusion.Types as Diffusion (Diffusion (..), StreamEntry (..))
import qualified Pos.Infra.Network.Policy as Policy
import           Pos.Infra.Network.Types (Bucket (..))
import           Pos.Infra.Reporting.Health.Types (HealthStatus (..))
import           Pos.Logic.Pure (pureLogic)
import           Pos.Logic.Types as Logic (Logic (..))

import           Pos.Core.Chrono (NewestFirst (..), OldestFirst (..))
import           Pos.Util.Trace (wlogTrace)
import           Test.Pos.Block.Arbitrary.Generate (generateMainBlock)

-- HLint warning disabled since I ran into https://ghc.haskell.org/trac/ghc/ticket/13106
-- when trying to resolve it.
{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

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

withTransport :: (Transport -> IO t) -> IO t
withTransport k = bracket InMemory.createTransport closeTransport k

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
      -- 'pureLogic' always gives an empty first component list, meaning all
      -- of the input is *not* in the main chain. This would cause streaming
      -- to fail, as it must have an intersection with the main chain from
      -- which to start.
    , getLcaMainChain = \(OldestFirst headers) ->
          pure (NewestFirst (reverse headers), OldestFirst [])
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

withServer :: ProtocolMagic -> Transport -> Logic IO -> (NodeId -> IO t) -> IO t
withServer pm transport logic k = do
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
        { fdcProtocolMagic = pm
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
    :: ProtocolMagic
    -> Word32
    -> Transport
    -> Logic IO
    -> NodeId
    -> (Diffusion IO -> IO t)
    -> IO t
withClient pm streamWindow transport logic serverAddress@(Node.NodeId _) k = do
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
        { fdcProtocolMagic = pm
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

-- Generate a list of n+1 blocks
generateBlocks :: ProtocolMagic -> Int -> NonEmpty Block
generateBlocks pm blocks =
  let root = doGenerateBlock 0 in
  root :| (doGenerateBlocks blocks)
  where
    doGenerateBlock :: Int -> Block
    doGenerateBlock seed =
        let size = 4 in
        force $ Right (generateMainBlock pm protocolConstants seed size)

    doGenerateBlocks :: Int -> [Block]
    doGenerateBlocks 0 = []
    doGenerateBlocks x = do
        [doGenerateBlock x] ++ (doGenerateBlocks (x-1))

streamSimple :: ProtocolMagic -> Word32 -> Int -> IO Bool
streamSimple pm streamWindow blocks = do
    streamIORef <- newIORef []
    resultIORef <- newIORef False
    let arbitraryBlocks = generateBlocks pm (blocks - 1)
        arbitraryHeaders = NE.map Core.getBlockHeader arbitraryBlocks
        arbitraryHashes = NE.map blockHeaderHash arbitraryHeaders
        !arbitraryBlock = NE.head arbitraryBlocks
        tipHash = NE.head arbitraryHashes
        checkpoints = [tipHash]
        setStreamIORef = \_ -> writeIORef streamIORef $ NE.tail arbitraryBlocks
    withTransport $ \transport ->
        withServer pm transport (serverLogic streamIORef arbitraryBlock arbitraryHashes arbitraryHeaders) $ \serverAddress ->
        withClient pm streamWindow transport clientLogic serverAddress $
            liftIO . blockDownloadStream serverAddress resultIORef streamIORef setStreamIORef
                (tipHash, checkpoints)
    readIORef resultIORef

batchSimple :: ProtocolMagic -> Int -> IO Bool
batchSimple pm blocks = do
    streamIORef <- newIORef []
    let arbitraryBlocks = generateBlocks pm (blocks - 1)
        arbitraryHeaders = NE.map Core.getBlockHeader arbitraryBlocks
        arbitraryHashes = NE.map blockHeaderHash arbitraryHeaders
        arbitraryBlock = NE.head arbitraryBlocks
        !checkPoints = if blocks == 1 then [someHash]
                                      else [someHash' (blocks + 1) []]
    withTransport $ \transport ->
        withServer pm transport (serverLogic streamIORef arbitraryBlock arbitraryHashes arbitraryHeaders) $ \serverAddress ->
        withClient pm 2048 transport clientLogic serverAddress $
            liftIO . blockDownloadBatch serverAddress (someHash, checkPoints)
    return True

spec :: Spec
spec = do
    runWithMagic NMMustBeNothing
    runWithMagic NMMustBeJust

runWithMagic :: RequiresNetworkMagic -> Spec
runWithMagic rnm = do
    pm <- (\ident -> ProtocolMagic ident rnm) <$> runIO (generate arbitrary)
    describe ("(requiresNetworkMagic=" ++ show rnm ++ ")") $
        describe "Blockdownload" $ do
            it "Stream 4 blocks" $ do
                r <- streamSimple pm 2048 4
                r `shouldBe` True
            it "Stream 128 blocks" $ do
                r <- streamSimple pm 2048 128
                r `shouldBe` True
            it "Stream 4096 blocks" $ do
                r <- streamSimple pm 128 4096
                r `shouldBe` True
            it "Streaming dislabed by client" $ do
                r <- streamSimple pm 0 4
                r `shouldBe` False
            it "Batch, single block" $ do
                r <- batchSimple pm 1
                r `shouldBe` True
            it "Batch of blocks" $ do
                r <- batchSimple pm 2200
                r `shouldBe` True
