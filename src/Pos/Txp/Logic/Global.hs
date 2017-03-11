{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Transaction processing logic.

module Pos.Txp.Logic.Global
       ( txVerifyBlocks
       , txApplyBlocks
       , txRollbackBlocks
       ) where

import           Control.Monad.Except (MonadError, runExceptT)
import qualified Data.HashMap.Strict  as HM
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
                                       ToilModifier (..), ToilT, ToilVerFailure, applyTxp,
                                       rollbackTxp, runDBTxp, runToilTGlobal, verifyTxp)

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
    fst <$> runToilAction (mapM (verifyTxp . getTxas) newChain)

-- | Apply chain of /definitely/ valid blocks to state on transactions
-- processing.
txApplyBlocks
    :: (MonadThrow m, TxpGlobalApplyMode m)
    => OldestFirst NE (Blund ssc)
    -> m SomeBatchOp
txApplyBlocks blunds = do
    let blocks = map fst blunds
    inAssertMode $ do
        verdict <- runExceptT $ txVerifyBlocks blocks
        case verdict of
            Right _ -> pass
            Left errors ->
                assertionFailed $
                "txVerifyBlocks failed in txApplyBlocks call: " <> pretty errors
    txpModifierToBatch . snd <$>
        runToilAction (mapM (applyTxp . blundToAuxNUndo) blunds)

-- | Rollback chain of blocks.
txRollbackBlocks
    :: (WithLogger m, MonadDB m)
    => NewestFirst NE (Blund ssc) -> m SomeBatchOp
txRollbackBlocks blunds =
    txpModifierToBatch . snd <$>
    runToilAction (mapM (rollbackTxp . blundToAuxNUndo) blunds)

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

-- Run action which requires toil interfaces.
runToilAction
    :: MonadDB m
    => ToilT (DBTxp m) a -> m (a, ToilModifier)
runToilAction action = runDBTxp . runToilTGlobal $ action

-- Zip block's TxAuxes and corresponding TxUndos.
blundToAuxNUndo :: Blund ssc -> [(TxAux, TxUndo)]
blundToAuxNUndo = uncurry zip . bimap getTxas undoTx

-- Get block's TxAuxes.
getTxas :: Block ssc -> [TxAux]
getTxas (Left _)   = []
getTxas (Right mb) = mb ^. blockTxas
