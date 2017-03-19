{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies    #-}

-- | All logic of Toil.  It operates in terms of MonadUtxo,
-- MonadToilEnv, MonadBalances and MonadTxPool.

module Pos.Txp.Toil.Logic
       ( GlobalToilMode
       , verifyToil
       , applyToil
       , rollbackToil

       , LocalToilMode
       , normalizeToil
       , processTx

       , verifyAndApplyTx
       ) where

import           Control.Monad.Except  (MonadError (..))
import           System.Wlog           (WithLogger)
import           Universum

import           Pos.Binary.Class      (biSize)
import           Pos.Constants         (maxLocalTxs)
import           Pos.Crypto            (WithHash (..), hash)

import           Pos.Txp.Core          (TxAux, TxId, TxUndo, TxpUndo, topsortTxs)
import           Pos.Txp.Toil.Balances (applyTxsToBalances, rollbackTxsBalances)
import           Pos.Txp.Toil.Class    (MonadBalances (..), MonadToilEnv (..),
                                        MonadTxPool (..), MonadUtxo (..))
import           Pos.Txp.Toil.Failure  (ToilVerFailure (..))
import           Pos.Txp.Toil.Types    (ToilEnv (teMaxTxSize))
import qualified Pos.Txp.Toil.Utxo     as Utxo

----------------------------------------------------------------------------
-- Global
----------------------------------------------------------------------------

type GlobalToilMode m = ( MonadUtxo m
                        , MonadBalances m
                        , MonadToilEnv m
                        , WithLogger m)

-- CHECK: @verifyToil
-- | Verify transactions correctness with respect to Utxo applying
-- them one-by-one.
-- Note: transactions must be topsorted to pass check.
-- Warning: this function may apply some transactions and fail
-- eventually. Use it only on temporary data.
verifyToil
    :: (GlobalToilMode m, MonadError ToilVerFailure m)
    => [TxAux] -> m TxpUndo
verifyToil = mapM (verifyAndApplyTx False . withTxId)

-- | Apply transactions from one block. They must be valid (for
-- example, it implies topological sort).
applyToil
    :: GlobalToilMode m
    => [(TxAux, TxUndo)]
    -> m ()
applyToil txun = do
    applyTxsToBalances txun
    mapM_ (applyTxToUtxo' . withTxId . fst) txun

-- | Rollback transactions from one block.
rollbackToil :: GlobalToilMode m => [(TxAux, TxUndo)] -> m ()
rollbackToil txun = do
    rollbackTxsBalances txun
    mapM_ Utxo.rollbackTxUtxo $ reverse txun

----------------------------------------------------------------------------
-- Local
----------------------------------------------------------------------------

type LocalToilMode m = ( MonadUtxo m
                      , MonadToilEnv m
                      , MonadTxPool m
                      )

-- CHECK: @processTx
-- #processWithPureChecks
-- | Verify one transaction and also add it to mem pool and apply to utxo
-- if transaction is valid.
processTx
    :: (LocalToilMode m, MonadError ToilVerFailure m)
    => (TxId, TxAux) -> m TxUndo
processTx tx@(id, aux) = do
    whenM (hasTx id) $ throwError ToilKnown
    whenM ((>= maxLocalTxs) <$> poolSize) $ throwError ToilOverwhelmed
    undo <- verifyAndApplyTx True tx
    undo <$ putTxWithUndo id aux undo

-- | Get rid of invalid transactions.
-- All valid transactions will be added to mem pool and applied to utxo.
normalizeToil
    :: (LocalToilMode m)
    => [(TxId, TxAux)]
    -> m ()
normalizeToil txs = mapM_ normalize ordered
  where
    ordered = fromMaybe txs $ topsortTxs wHash txs
    wHash (i, (t, _, _)) = WithHash t i
    normalize = runExceptT . processTx

----------------------------------------------------------------------------
-- ToilEnv logic
----------------------------------------------------------------------------

verifyToilEnv
    :: (MonadToilEnv m, MonadError ToilVerFailure m)
    => TxAux -> m ()
verifyToilEnv txAux = do
    limit <- teMaxTxSize <$> getToilEnv
    let txSize = biSize txAux
    when (txSize > limit) $
        throwError ToilTooLargeTx {ttltSize = txSize, ttltLimit = limit}

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

verifyAndApplyTx
    :: (MonadUtxo m, MonadToilEnv m, MonadError ToilVerFailure m)
    => Bool -> (TxId, TxAux) -> m TxUndo
verifyAndApplyTx verifyVersions tx@(_, txAux) = do
    verifyToilEnv txAux
    Utxo.verifyTxUtxo ctx txAux <* applyTxToUtxo' tx
  where
    ctx = Utxo.VTxContext verifyVersions

withTxId :: TxAux -> (TxId, TxAux)
withTxId aux@(tx, _, _) = (hash tx, aux)

applyTxToUtxo' :: MonadUtxo m => (TxId, TxAux) -> m ()
applyTxToUtxo' (i, (t, _, d)) = Utxo.applyTxToUtxo (WithHash t i) d
