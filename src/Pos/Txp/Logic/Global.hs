{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP                 #-}

-- | Transaction processing logic.

module Pos.Txp.Logic.Global
       ( txVerifyBlocks
       , txApplyBlocks
       , txRollbackBlocks
       ) where

import           Control.Monad.Except (MonadError, runExceptT)
import qualified Data.HashMap.Strict  as HM
import           Formatting           (build, sformat, (%))
import           System.Wlog          (WithLogger)
import           Universum

import           Pos.Block.Types      (Blund, Undo (undoTx))
import           Pos.DB               (MonadDB, SomeBatchOp (..))
import qualified Pos.DB.GState        as GS
import           Pos.Exception        (assertionFailed)
import           Pos.Txp.Core.Types   (TxAux, TxUndo, TxpUndo)
import           Pos.Types            (Block, blockTxas)
import           Pos.Util             (NE, NewestFirst (..), OldestFirst (..),
                                       inAssertMode)
import qualified Pos.Util.Modifier    as MM

import           Pos.Txp.Toil         (BalancesView (..), BalancesView (..), DBTxp,
                                       ToilModifier (..), ToilT, ToilVerFailure, applyToil,
                                       rollbackToil, runDBTxp, runToilTGlobal, verifyToil)
#ifdef WITH_EXPLORER
import           Pos.Types              (BiSsc, HeaderHash, Timestamp, headerHash)
import           Pos.Txp.Toil           (MemPool (..))
#endif

type TxpGlobalApplyMode m = ( WithLogger m
                            , MonadDB m
                            )

type TxpGlobalVerifyMode m = ( WithLogger m
                             , MonadDB m
                             , MonadError ToilVerFailure m
                             )

-- | Verify chain of blocks and return transaction undos of blocks.
txVerifyBlocks
    :: forall ssc m . TxpGlobalVerifyMode m
    => OldestFirst NE (Block ssc)
    -> m (OldestFirst NE TxpUndo)
txVerifyBlocks newChain =
    fst <$> runToilAction (mapM (verifyToil . getTxas) newChain)

-- | Apply chain of /definitely/ valid blocks to state on transactions
-- processing.
#ifdef WITH_EXPLORER
txApplyBlocks
    :: (MonadThrow m, TxpGlobalApplyMode m, BiSsc ssc)
    => OldestFirst NE (Blund ssc)
    -> Timestamp
    -> m SomeBatchOp
txApplyBlocks blunds curTime = do
#else
txApplyBlocks
    :: (MonadThrow m, TxpGlobalApplyMode m)
    => OldestFirst NE (Blund ssc)
    -> m SomeBatchOp
txApplyBlocks blunds = do
#endif
    let blocks = map fst blunds
    inAssertMode $ do
        verdict <- runExceptT $ txVerifyBlocks blocks
        whenLeft verdict $ assertionFailed .
            sformat ("txVerifyBlocks failed in txApplyBlocks call: "%build)
    txpModifierToBatch . snd <$> runToilAction
#ifdef WITH_EXPLORER
        (mapM (uncurry (applyToil curTime) . blundToAuxNUndoWHash) blunds)
#else
        (mapM (applyToil . blundToAuxNUndo) blunds)
#endif

-- | Rollback chain of blocks.
txRollbackBlocks
    :: (WithLogger m, MonadDB m)
    => NewestFirst NE (Blund ssc) -> m SomeBatchOp
txRollbackBlocks blunds =
    txpModifierToBatch . snd <$>
    runToilAction (mapM (rollbackToil . blundToAuxNUndo) blunds)

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

-- Convert ToilModifier to batch of database operations.
txpModifierToBatch :: ToilModifier -> SomeBatchOp
txpModifierToBatch (ToilModifier um (BalancesView (HM.toList -> stakes) total)
#ifdef WITH_EXPLORER
                       (MemPool _ _ em (HM.toList -> histories))
#else
                       _
#endif
                    _) =
#ifdef WITH_EXPLORER
    SomeBatchOp [SomeBatchOp utxoOps, SomeBatchOp balancesOps, SomeBatchOp explorerOps]
#else
    SomeBatchOp [SomeBatchOp utxoOps, SomeBatchOp balancesOps]
#endif
  where
    utxoOps =
        map GS.DelTxIn (MM.deletions um) ++
        map (uncurry GS.AddTxOut) (MM.insertions um)
    balancesOpsAlmost = map (uncurry GS.PutFtsStake) stakes
    balancesOps = case total of Nothing -> balancesOpsAlmost
                                Just x -> GS.PutFtsSum x : balancesOpsAlmost
#ifdef WITH_EXPLORER
    explorerOps =
          map GS.DelTxExtra (MM.deletions em) ++
          map (uncurry GS.AddTxExtra) (MM.insertions em) ++
          map (uncurry GS.UpdateAddrHistory) histories
#endif

-- Run action which requires toil interfaces.
runToilAction
    :: MonadDB m
    => ToilT (DBTxp m) a -> m (a, ToilModifier)
runToilAction action = runDBTxp . runToilTGlobal $ action

-- Zip block's TxAuxes and corresponding TxUndos.
blundToAuxNUndo :: Blund ssc -> [(TxAux, TxUndo)]
blundToAuxNUndo = uncurry zip . bimap getTxas undoTx

#ifdef WITH_EXPLORER
-- Zip block's TxAuxes and also add block hash
blundToAuxNUndoWHash :: BiSsc ssc => Blund ssc -> ([(TxAux, TxUndo)], HeaderHash)
blundToAuxNUndoWHash blund = (blundToAuxNUndo blund, headerHash blund)
#endif

-- Get block's TxAuxes.
getTxas :: Block ssc -> [TxAux]
getTxas (Left _)   = []
getTxas (Right mb) = mb ^. blockTxas
