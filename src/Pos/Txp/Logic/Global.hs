{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
                                      ToilModifier (..), ToilT, ToilVerFailure, applyTxp,
                                      rollbackTxp, runDBTxp, runToilTGlobal, verifyTxp)
import qualified Pos.Util.Modifier   as MM

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
    :: TxpWorkMode m
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
                   mapM (applyTxp . blundToAuxNUndo) $ blunds
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

-- Convert ToilModifier to batch of database operations.
txpModifierToBatch :: ToilModifier -> SomeBatchOp
txpModifierToBatch (ToilModifier um (BalancesView (HM.toList -> stakes) total) _ _) =
    SomeBatchOp [SomeBatchOp utxoOps, SomeBatchOp balancesOps]
  where
    utxoOps =
        map GS.DelTxIn (MM.deletions um) ++
        map (uncurry GS.AddTxOut) (MM.insertions um)
    balancesOps =
        maybe identity (\x l -> (GS.PutFtsSum x : l)) total $
        map (uncurry GS.PutFtsStake) stakes

-- Run action which requires MonadUtxo and MonadTxPool interfaces.
runTxpAction
    :: MonadDB m
    => ToilT (DBTxp (ExceptT ToilVerFailure m)) a
    -> m (Either ToilVerFailure (a, ToilModifier))
runTxpAction action = runExceptT . runDBTxp . runToilTGlobal $ action

-- Zip block's TxAuxes and corresponding TxUndos.
blundToAuxNUndo :: Blund ssc -> [(TxAux, TxUndo)]
blundToAuxNUndo = uncurry zip . bimap getTxas undoTx

-- Get block's TxAuxes.
getTxas :: Block ssc -> [TxAux]
getTxas (Left _)   = []
getTxas (Right mb) = mb ^. blockTxas
