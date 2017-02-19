{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Transaction processing logic.

module Pos.Txp.Logic.Global
       ( txVerifyBlocks
       , txApplyBlocks
       , txRollbackBlocks
       ) where

import           Control.Lens        (_Wrapped)
import           Data.Default        (def)
import qualified Data.List.NonEmpty  as NE
import           System.Wlog         (WithLogger)
import           Universum

import           Pos.Block.Types     (Blund, Undo (undoTx))
import           Pos.DB              (MonadDB, SomeBatchOp (..))
import qualified Pos.DB.GState       as GS
import           Pos.Exception       (assertionFailed)
import           Pos.Ssc.Class.Types (Ssc)
import           Pos.Types           (Block, TxAux, TxUndo, TxsUndo, blockTxas,
                                      headerHash, prevBlockL)
import           Pos.Util            (NE, NewestFirst (..), OldestFirst (..),
                                      inAssertMode, _neHead)

import           Pos.Txp.Error       (TxpError (..))
import           Pos.Txp.MemState    (MonadTxpMem (..))
import           Pos.Txp.Txp         (BalancesView (..), DBTxp, TxpModifier (..), TxpT,
                                      TxpVerFailure, applyTxp, rollbackTxp, runDBTxp,
                                      runTxpT, verifyTxp)
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
    tip <- GS.getTip
    when (tip /= blocks ^. _Wrapped . _neHead . prevBlockL) $
        throwEx "oldest block in 'blunds' is not based on tip"
    inAssertMode $
        do verdict <- txVerifyBlocks blocks
           case verdict of
               Right _     -> pass
               Left errors ->
                   assertionFailed $
                   "txVerifyBlocks failed in txApplyBlocks call: " <> errors
    verdict <- runTxpAction $
                   mapM (applyTxp . blundToAuxNUndo) $ blunds
    let putTip = SomeBatchOp $
                 GS.PutTip $
                 headerHash $
                 NE.last $
                 getOldestFirst blunds
    case verdict of
        Left er           -> throwEx $ pretty er
        Right (_, txpMod) ->
            pure $ SomeBatchOp [putTip, txpModifierToBatch txpMod]
  where
    throwEx = throwM . TxpInternalError . (<>) "txApplyBlocks failed: "

-- | Rollback chain of blocks.
txRollbackBlocks
    :: (WithLogger m, MonadDB ssc m)
    => NewestFirst NE (Blund ssc) -> m SomeBatchOp
txRollbackBlocks blunds = do
    verdict <- runTxpAction $
                   mapM (rollbackTxp . blundToAuxNUndo) $ blunds
    let putTip = SomeBatchOp $
                 GS.PutTip $
                 headerHash $
                 (NE.last $ getNewestFirst blunds) ^. prevBlockL
    case verdict of
        Left er           -> throwEx $ pretty er
        Right (_, txpMod) ->
            pure $ SomeBatchOp [putTip, txpModifierToBatch txpMod]
  where
    throwEx = throwM . TxpInternalError . (<>) "txRollbackBlocks failed: "

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

txpModifierToBatch :: TxpModifier -> SomeBatchOp
txpModifierToBatch = notImplemented

runTxpAction
    :: MonadDB ssc m
    => TxpT (DBTxp (ExceptT TxpVerFailure m)) a -> m (Either TxpVerFailure (a, TxpModifier))
runTxpAction action = do
    total <- GS.getTotalFtsStake
    let balView = BalancesView mempty total
    runExceptT . runDBTxp . runTxpT (TxpModifier def balView) $ action

blundToAuxNUndo :: Blund ssc -> [(TxAux, TxUndo)]
blundToAuxNUndo = uncurry zip . bimap getTxas undoTx

getTxas :: Block ssc -> [TxAux]
getTxas (Left _)   = []
getTxas (Right mb) = mb ^. blockTxas
