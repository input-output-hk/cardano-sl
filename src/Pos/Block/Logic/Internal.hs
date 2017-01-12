{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Internal block logic. Mostly needed for use in 'Pos.Lrc' -- using
-- lrc requires to apply and rollback blocks, but applying many blocks
-- requires triggering lrc recalculations.

module Pos.Block.Logic.Internal
       ( applyBlocksUnsafe
       , rollbackBlocksUnsafe
       , withBlkSemaphore
       , withBlkSemaphore_
       ) where

import           Control.Arrow        ((&&&))
import           Control.Lens         (view, (^.), _1)
import           Control.Monad.Catch  (bracketOnError)
import           Data.List.NonEmpty   (NonEmpty)
import qualified Data.List.NonEmpty   as NE
import           Universum

import           Pos.Context          (lrcActionOnEpochReason, putBlkSemaphore,
                                       takeBlkSemaphore)
import           Pos.Crypto           (hash)
import           Pos.DB               (SomeBatchOp (..))
import qualified Pos.DB               as DB
import qualified Pos.DB.GState        as GS
import qualified Pos.DB.Lrc           as DB
import           Pos.Delegation.Logic (delegationApplyBlocks, delegationRollbackBlocks)
import           Pos.Ssc.Extra        (sscApplyBlocks, sscApplyGlobalState, sscRollback)
import           Pos.Txp.Logic        (normalizeTxpLD, txApplyBlocks, txRollbackBlocks)
import           Pos.Types            (Blund, HeaderHash, blockHeader, epochIndexL,
                                       headerHashG, prevBlockL)
import           Pos.Util             (spanSafe, _neLast)
import           Pos.WorkMode         (WorkMode)


-- | Run action acquiring lock on block application. Argument of
-- action is an old tip, result is put as a new tip.
withBlkSemaphore
    :: WorkMode ssc m
    => (HeaderHash ssc -> m (a, HeaderHash ssc)) -> m a
withBlkSemaphore action =
    bracketOnError takeBlkSemaphore putBlkSemaphore doAction
  where
    doAction tip = do
        (res, newTip) <- action tip
        res <$ putBlkSemaphore newTip

-- | Version of withBlkSemaphore which doesn't have any result.
withBlkSemaphore_
    :: WorkMode ssc m
    => (HeaderHash ssc -> m (HeaderHash ssc)) -> m ()
withBlkSemaphore_ = withBlkSemaphore . (fmap ((), ) .)

-- | Applies definitely valid prefix of blocks -- that has the same
-- epoch index. This function is unsafe, use it only if you understand
-- what you're doing. That means you can break system guarantees.
applyBlocksUnsafe
    :: forall ssc m . WorkMode ssc m => NonEmpty (Blund ssc) -> m ()
applyBlocksUnsafe blunds0 = do
    -- Note: it's important to put blocks first
    mapM_ putToDB blunds
    delegateBatch <- SomeBatchOp <$> delegationApplyBlocks blocks
    txBatch <- SomeBatchOp <$> txApplyBlocks blunds
    sscApplyBlocks blocks
    let epoch = blunds ^. _neLast . _1 . epochIndexL
    richmen <-
        lrcActionOnEpochReason epoch "couldn't get SSC richmen" DB.getRichmenSsc
    sscApplyGlobalState richmen
    GS.writeBatchGState [delegateBatch, txBatch, forwardLinksBatch]
    normalizeTxpLD
  where
    -- hehe it's not unsafe yet TODO
    (blunds,_) = spanSafe (\(h,_) (b,_) -> b ^. epochIndexL == h ^. epochIndexL) blunds0
    blocks = fmap fst blunds
    forwardLinks = map (view prevBlockL &&& view headerHashG) $ NE.toList blocks
    forwardLinksBatch = SomeBatchOp $ map (uncurry GS.BlockExtraAddForwardLink) forwardLinks
    putToDB (blk, undo) = DB.putBlock undo True blk

-- | Rollback sequence of blocks, head-newest order exepected with
-- head being current tip. It's also assumed that lock on block db is
-- taken.  application is taken already.
rollbackBlocksUnsafe :: (WorkMode ssc m) => NonEmpty (Blund ssc) -> m ()
rollbackBlocksUnsafe toRollback = do
    -- [CSL-378] Update sbInMain properly (in transaction)
    delRoll <- SomeBatchOp <$> delegationRollbackBlocks toRollback
    txRoll <- SomeBatchOp <$> txRollbackBlocks toRollback
    forM_ (NE.toList toRollback) $
        \(blk,_) -> DB.setBlockInMainChain (hash $ blk ^. blockHeader) False
    sscRollback $ fmap fst toRollback
    GS.writeBatchGState [delRoll, txRoll, forwardLinksBatch]
  where
    forwardLinksBatch =
        SomeBatchOp $
        map (GS.BlockExtraRemoveForwardLink . view prevBlockL . fst)
            (NE.toList toRollback)
