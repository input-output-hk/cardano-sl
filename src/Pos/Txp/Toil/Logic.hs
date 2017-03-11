{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies    #-}

-- | All logic of Txp,
-- it operates in terms of MonadUtxo, MonadBalances and MonadTxPool.

module Pos.Txp.Toil.Logic
       ( verifyTxp
       , applyTxp
       , rollbackTxp
       , normalizeTxp
       , processTx
       ) where

import           Control.Monad.Except  (MonadError (..))
import           System.Wlog           (WithLogger)
import           Universum

import           Pos.Constants         (maxLocalTxs)
import           Pos.Crypto            (WithHash (..), hash)

import           Pos.Txp.Core          (TxAux, TxId, TxUndo, TxpUndo, topsortTxs)
import           Pos.Txp.Toil.Balances (applyTxsToBalances, rollbackTxsBalances)
import           Pos.Txp.Toil.Class    (MonadBalances (..), MonadTxPool (..),
                                        MonadUtxo (..))
import           Pos.Txp.Toil.Failure  (ToilVerFailure (..))
import qualified Pos.Txp.Toil.Utxo     as Utxo

----------------------------------------------------------------------------
-- Global
----------------------------------------------------------------------------

type GlobalTxpMode m = ( MonadUtxo m
                       , MonadBalances m
                       , WithLogger m)

-- CHECK: @verifyTxp
-- | Verify transactions correctness with respect to Utxo applying
-- them one-by-one.
-- Note: transactions must be topsorted to pass check.
-- Warning: this function may apply some transactions and fail
-- eventually. Use it only on temporary data.
verifyTxp
    :: (GlobalTxpMode m, MonadError ToilVerFailure m)
    => [TxAux] -> m TxpUndo
verifyTxp = mapM (verifyAndApplyTx False . withTxId)

-- | Apply transactions from one block. They must be valid (for
-- example, it implies topological sort).
applyTxp :: GlobalTxpMode m => [(TxAux, TxUndo)] -> m ()
applyTxp txun = do
    applyTxsToBalances txun
    mapM_ (applyTxToUtxo' . withTxId . fst) txun

-- | Rollback transactions from one block.
rollbackTxp :: GlobalTxpMode m => [(TxAux, TxUndo)] -> m ()
rollbackTxp txun = do
    rollbackTxsBalances txun
    mapM_ Utxo.rollbackTxUtxo $ reverse txun

----------------------------------------------------------------------------
-- Local
----------------------------------------------------------------------------

type LocalTxpMode m = ( MonadUtxo m
                      , MonadTxPool m
                      )

-- CHECK: @processTx
-- #processWithPureChecks
-- | Verify one transaction and also add it to mem pool and apply to utxo
-- if transaction is valid.
processTx
    :: (LocalTxpMode m, MonadError ToilVerFailure m)
    => (TxId, TxAux) -> m ()
processTx tx@(id, aux) = do
    whenM (hasTx id) $ throwError ToilKnown
    whenM ((>= maxLocalTxs) <$> poolSize) $ throwError ToilOverwhelmed
    undo <- verifyAndApplyTx True tx
    putTxWithUndo id aux undo

-- | Get rid of invalid transactions.
-- All valid transactions will be added to mem pool and applied to utxo.
normalizeTxp
    :: (LocalTxpMode m)
    => [(TxId, TxAux)] -> m ()
normalizeTxp txs = mapM_ (runExceptT . processTx) ordered
  where
    ordered = fromMaybe txs $ topsortTxs wHash txs
    wHash (i, (t, _, _)) = WithHash t i

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

verifyAndApplyTx
    :: (MonadUtxo m, MonadError ToilVerFailure m)
    => Bool -> (TxId, TxAux) -> m TxUndo
verifyAndApplyTx verifyVersions tx@(_, txAux) =
    Utxo.verifyTxUtxo ctx txAux <* applyTxToUtxo' tx
  where
    ctx = Utxo.VTxContext verifyVersions

withTxId :: TxAux -> (TxId, TxAux)
withTxId aux@(tx, _, _) = (hash tx, aux)

applyTxToUtxo' :: MonadUtxo m => (TxId, TxAux) -> m ()
applyTxToUtxo' (i, (t, _, d)) = Utxo.applyTxToUtxo (WithHash t i) d
