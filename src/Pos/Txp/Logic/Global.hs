{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Transaction processing logic.

module Pos.Txp.Logic.Global
       ( txVerifyBlocks
       , txApplyBlocks
       , txRollbackBlocks
       ) where

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet        as HS
import           System.Wlog         (WithLogger)
import           Universum

import           Pos.Block.Types     (Blund, Undo (undoTx))
import           Pos.DB              (MonadDB, SomeBatchOp (..))
import qualified Pos.DB.GState       as GS
import           Pos.Exception       (assertionFailed)
import           Pos.Ssc.Class.Types (Ssc)
import           Pos.Txp.Core.Types  (TxAux, TxUndo, TxsUndo)
import           Pos.Types           (Block, blockTxas)
import           Pos.Util            (NE, NewestFirst (..), OldestFirst (..),
                                      inAssertMode)

import           Pos.Txp.Error       (TxpError (..))
import           Pos.Txp.MemState    (MonadTxpMem (..))
import           Pos.Txp.Toil        (BalancesView (..), BalancesView (..), DBTxp,
                                      TxpModifier (..), TxpT, TxpVerFailure,
                                      UtxoView (..), applyTxp, rollbackTxp, runDBTxp,
                                      runTxpTGlobal, verifyTxp)
type TxpWorkMode ssc m = ( Ssc ssc
                         , WithLogger m
                         , MonadDB ssc m
                         , MonadTxpMem m
                         , MonadThrow m)

-- | Verify chain of blocks and return transaction undos of blocks.
txVerifyBlocks
    :: forall ssc m . TxpWorkMode ssc m
    => OldestFirst NE (Block ssc)
    -> m (Either Text (OldestFirst NE TxsUndo))
txVerifyBlocks newChain = do
    verdict <- runTxpAction $
                   mapM (verifyTxp . getTxas) newChain
    case verdict of
        Left er          -> pure $ Left $ pretty er
        Right (undos, _) -> pure $ Right undos

-- | Apply chain of /definitely/ valid blocks to state on transactions
-- processing.
txApplyBlocks
    :: TxpWorkMode ssc m
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
    :: (WithLogger m, MonadDB ssc m)
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
txpModifierToBatch (TxpModifier
                      (UtxoView (HM.toList -> addS) (HS.toList -> delS))
                      (BalancesView (HM.toList -> stakes) total) _ _) =
    SomeBatchOp [
          SomeBatchOp $ map GS.DelTxIn delS ++ map (uncurry GS.AddTxOut) addS
        , SomeBatchOp $ map (uncurry GS.PutFtsStake) stakes ++ [GS.PutFtsSum total]
                ]

-- Run action which requires MonadUtxo and MonadTxPool interfaces.
runTxpAction
    :: MonadDB ssc m
    => TxpT (DBTxp (ExceptT TxpVerFailure m)) a -> m (Either TxpVerFailure (a, TxpModifier))
runTxpAction action = do
    total <- GS.getTotalFtsStake
    let balView = BalancesView mempty total
    runExceptT . runDBTxp . runTxpTGlobal balView $ action

-- Zip block's TxAuxes and corresponding TxUndos.
blundToAuxNUndo :: Blund ssc -> [(TxAux, TxUndo)]
blundToAuxNUndo = uncurry zip . bimap getTxas undoTx

-- Get block's TxAuxes.
getTxas :: Block ssc -> [TxAux]
getTxas (Left _)   = []
getTxas (Right mb) = mb ^. blockTxas
