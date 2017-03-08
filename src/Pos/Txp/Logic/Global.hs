{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP                 #-}

-- | Transaction processing logic.

module Pos.Txp.Logic.Global
       ( txVerifyBlocks
       , txApplyBlocks
       , txRollbackBlocks
       ) where

import qualified Data.HashMap.Strict as HM
import           System.Wlog         (WithLogger)
import           Universum

import           Pos.Block.Types     (Blund, Undo (undoTx))
import           Pos.DB              (MonadDB, SomeBatchOp (..))
import qualified Pos.DB.GState       as GS
import           Pos.Exception       (assertionFailed)
import           Pos.Txp.Core.Types  (TxAux, TxUndo, TxpUndo)
import           Pos.Types           (Block, blockTxas)
import           Pos.Util            (NE, NewestFirst (..), OldestFirst (..),
                                      inAssertMode)

import           Pos.Txp.Error       (TxpError (..))
import           Pos.Txp.MemState    (MonadTxpMem (..))
import           Pos.Txp.Toil        (BalancesView (..), BalancesView (..), DBTxp,
                                      TxpModifier (..), TxpT, TxpVerFailure, applyTxp,
                                      rollbackTxp, runDBTxp, runTxpTGlobal, verifyTxp)
import qualified Pos.Util.Modifier   as MM
#ifdef WITH_EXPLORER
import           Pos.Types           (BiSsc, HeaderHash, headerHash)
import           Pos.Txp.Toil        (MemPool (..))
#endif

type TxpWorkMode m = (WithLogger m, MonadDB m, MonadTxpMem m, MonadThrow m)

-- | Verify chain of blocks and return transaction undos of blocks.
txVerifyBlocks
    :: forall ssc m . TxpWorkMode m
    => OldestFirst NE (Block ssc)
    -> m (Either Text (OldestFirst NE TxpUndo))
txVerifyBlocks newChain = do
    verdict <- runTxpAction $
                   mapM (verifyTxp . getTxas) newChain
    case verdict of
        Left er          -> pure $ Left $ pretty er
        Right (undos, _) -> pure $ Right undos

-- | Apply chain of /definitely/ valid blocks to state on transactions
-- processing.
txApplyBlocks
#ifdef WITH_EXPLORER
    :: (TxpWorkMode m, BiSsc ssc)
#else
    :: TxpWorkMode m
#endif
    => OldestFirst NE (Blund ssc)
    -> m SomeBatchOp
txApplyBlocks blunds = do
    let blocks = map fst blunds
    inAssertMode $
        do verdict <- txVerifyBlocks blocks
           case verdict of
               Right _     -> pass
               Left errors ->
                   assertionFailed $
                   "txVerifyBlocks failed in txApplyBlocks call: " <> errors
    verdict <- runTxpAction $
#ifdef WITH_EXPLORER
                   mapM (uncurry applyTxp . blundToAuxNUndoWHash) blunds
#else
                   mapM (applyTxp . blundToAuxNUndo) blunds
#endif
    case verdict of
        Left er           -> throwEx $ pretty er
        Right (_, txpMod) ->
            pure $ txpModifierToBatch txpMod
  where
    throwEx = throwM . TxpInternalError . (<>) "txApplyBlocks failed: "

-- | Rollback chain of blocks.
txRollbackBlocks
    :: (WithLogger m, MonadDB m)
    => NewestFirst NE (Blund ssc) -> m SomeBatchOp
txRollbackBlocks blunds = do
    verdict <- runTxpAction $
                   mapM (rollbackTxp . blundToAuxNUndo) $ blunds
    case verdict of
        Left er           -> throwEx $ pretty er
        Right (_, txpMod) -> pure $ txpModifierToBatch txpMod
  where
    throwEx = throwM . TxpInternalError . (<>) "txRollbackBlocks failed: "

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

-- Convert TxpModifier to batch of database operations.
txpModifierToBatch :: TxpModifier -> SomeBatchOp
txpModifierToBatch (TxpModifier um
                       (BalancesView (HM.toList -> stakes) total)
#ifdef WITH_EXPLORER
                       (MemPool _ _ (HM.toList -> extras)) _) =
#else
                       _ _) =
#endif
    SomeBatchOp
        [ SomeBatchOp $
          map GS.DelTxIn (MM.deletions um) ++
          map (uncurry GS.AddTxOut) (MM.insertions um)
        , SomeBatchOp $
          map (uncurry GS.PutFtsStake) stakes ++ [GS.PutFtsSum total]
#ifdef WITH_EXPLORER
        , SomeBatchOp $
          map (uncurry GS.AddTxExtra) extras
#endif
        ]

-- Run action which requires MonadUtxo and MonadTxPool interfaces.
runTxpAction
    :: MonadDB m
    => TxpT (DBTxp (ExceptT TxpVerFailure m)) a
    -> m (Either TxpVerFailure (a, TxpModifier))
runTxpAction action = do
    total <- GS.getTotalFtsStake
    let balView = BalancesView mempty total
    runExceptT . runDBTxp . runTxpTGlobal balView $ action

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
