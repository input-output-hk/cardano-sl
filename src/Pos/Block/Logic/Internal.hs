{-# LANGUAGE RankNTypes #-}

-- | Internal block logic. Mostly needed for use in 'Pos.Lrc' -- using
-- lrc requires to apply and rollback blocks, but applying many blocks
-- requires triggering lrc recalculations.

module Pos.Block.Logic.Internal
       ( applyBlocksUnsafe
       , rollbackBlocksUnsafe
       , withBlkSemaphore
       , withBlkSemaphore_
       ) where

import           Control.Lens         ((^.))
import           Control.Monad.Catch  (bracketOnError)
import           Data.List.NonEmpty   (NonEmpty)
import qualified Data.List.NonEmpty   as NE
import           Universum

import           Pos.Context          (putBlkSemaphore, takeBlkSemaphore)
import           Pos.Crypto           (hash)
import           Pos.DB               (SomeBatchOp (..))
import qualified Pos.DB               as DB
import qualified Pos.DB.GState        as GS
import           Pos.Delegation.Logic (delegationApplyBlocks, delegationRollbackBlocks)
import           Pos.Ssc.Extra        (sscApplyBlocks, sscApplyGlobalState, sscRollback)
import           Pos.Txp.Logic        (normalizeTxpLD, txApplyBlocks, txRollbackBlocks)
import           Pos.Types            (Blund, HeaderHash, blockHeader, epochIndexL)
import           Pos.Util             (spanSafe)
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
    sscApplyGlobalState
    GS.writeBatchGState [delegateBatch, txBatch]
    normalizeTxpLD
  where
    -- hehe it's not unsafe yet TODO
    (blunds,_) = spanSafe (\(h,_) (b,_) -> b ^. epochIndexL == h ^. epochIndexL) blunds0
    blocks = fmap fst blunds
    putToDB (blk, undo) = DB.putBlock undo True blk

-- | Rollback sequence of blocks, head block corresponds to tip,
-- further blocks are parents. It's assumed that lock on block
-- application is taken.
rollbackBlocksUnsafe :: (WorkMode ssc m) => NonEmpty (Blund ssc) -> m ()
rollbackBlocksUnsafe toRollback = do
    -- [CSL-378] Update sbInMain properly (in transaction)
    delRoll <- delegationRollbackBlocks toRollback
    txRoll <- txRollbackBlocks toRollback
    forM_ (NE.toList toRollback) $
        \(blk,_) -> DB.setBlockInMainChain (hash $ blk ^. blockHeader) False
    sscRollback $ fmap fst toRollback
    GS.writeBatchGState [delRoll, txRoll]
