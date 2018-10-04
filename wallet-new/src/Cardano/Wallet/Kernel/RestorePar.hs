module Cardano.Wallet.Kernel.RestorePar (
      resolveMainBlocks
    , allBlundsForEpoch
    , blundsToResolvedBlocks
    ) where

import           Universum

import qualified Control.Concurrent.Async as Async
import           Data.Maybe (fromJust)

import           Pos.Chain.Block (Block, Blund, HeaderHash, MainBlock, Undo,
                     mainBlockSlot, undoTx)
import           Pos.Chain.Genesis (configGenesisHash)
import           Pos.Core (getCurrentTimestamp)
import           Pos.Core.Chrono (NewestFirst)
import           Pos.Core.Slotting (EpochIndex, EpochOrSlot (..),
                     SlotId (siEpoch), getEpochOrSlot)
import           Pos.DB.Block (loadBlundsWhile)

import qualified Data.Conduit as CL
import qualified Data.Conduit.Combinators as CL

import           Cardano.Wallet.Kernel.DB.BlockContext (mainBlockContext)
import           Cardano.Wallet.Kernel.DB.Resolved (ResolvedBlock)
import           Cardano.Wallet.Kernel.NodeStateAdaptor (NodeStateAdaptor,
                     defaultGetSlotStart, getCoreConfig, withNodeState)
import           Cardano.Wallet.Kernel.Types (RawResolvedBlock (..),
                     fromRawResolvedBlock, rawResolvedBlock,
                     rawResolvedBlockInputs, rawResolvedContext, rawTimestamp)


blundsToResolvedBlocks :: NodeStateAdaptor IO
                       -> [Blund]
                       -> IO [ResolvedBlock]
blundsToResolvedBlocks node blunds =
    let blocks = CL.runConduit $ do
                       CL.yieldMany blunds
                 CL..| CL.mapWhile mainBlock
                 CL..| CL.sinkList
    in resolveMainBlocks node (runIdentity blocks)
    where
        mainBlock :: Blund -> Maybe (MainBlock, Undo)
        mainBlock (Left _, _)   = Nothing
        mainBlock (Right mb, u) = Just (mb, u)

resolveMainBlocks :: NodeStateAdaptor IO
                  -> [(MainBlock, Undo)]
                  -> IO [ResolvedBlock]
resolveMainBlocks node blocks = do
    genesisHash <- configGenesisHash <$> getCoreConfig node
    Async.forConcurrently blocks $ \(mainBlock, u) -> do
        withNodeState node $ \_lock -> do
            ctxt  <- mainBlockContext genesisHash mainBlock
            mTime <- defaultGetSlotStart (mainBlock ^. mainBlockSlot)
            now   <- liftIO $ getCurrentTimestamp
            return $ fromRawResolvedBlock UnsafeRawResolvedBlock {
                rawResolvedBlock       = mainBlock
              , rawResolvedBlockInputs = map (map fromJust) $ undoTx u
              , rawTimestamp           = either (const now) identity mTime
              , rawResolvedContext     = ctxt
              }

-- | When given as input an 'EpochIndex' and the 'HeaderHash' of where this
-- 'EpochIndex' starts, returns all the 'Blund's between 'EpochIndex' and
-- 'EpochIndex + 1'.
allBlundsForEpoch :: NodeStateAdaptor IO
                  -> EpochIndex
                  -> HeaderHash
                  -> IO (NewestFirst [] Blund)
allBlundsForEpoch node targetEpoch epochStart = do
    genesisHash <- configGenesisHash <$> getCoreConfig node
    withNodeState node $ \_lock -> do
        loadBlundsWhile genesisHash thisEpoch epochStart
  where
      thisEpoch :: Block -> Bool
      thisEpoch b =
          case getEpochOrSlot b of
               (EpochOrSlot (Left epochIndex)) ->
                   epochIndex == targetEpoch
               (EpochOrSlot (Right slotId))    ->
                   (siEpoch slotId) == targetEpoch


