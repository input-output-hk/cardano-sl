{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies    #-}

-- | All logic of Txp,
-- it operates in terms of MonadUtxo, MonadBalances and MonadTxPool.

module Pos.Txp.Toil.Logic
       ( verifyToil
       , applyToil
       , rollbackToil
       , normalizeToil
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
#ifdef WITH_EXPLORER
import           Pos.Txp.Toil.Class   (MonadTxExtra (..), MonadTxExtraRead (..))
import           Pos.Types            (TxExtra (..), HeaderHash, Timestamp)
#endif

----------------------------------------------------------------------------
-- Global
----------------------------------------------------------------------------

type GlobalTxpMode m = ( MonadUtxo m
                       , MonadBalances m
#ifdef WITH_EXPLORER
                       , MonadTxExtra m
#endif
                       , WithLogger m)

-- CHECK: @verifyToil
-- | Verify transactions correctness with respect to Utxo applying
-- them one-by-one.
-- Note: transactions must be topsorted to pass check.
-- Warning: this function may apply some transactions and fail
-- eventually. Use it only on temporary data.
verifyToil
    :: (GlobalTxpMode m, MonadError ToilVerFailure m)
    => [TxAux] -> m TxpUndo
verifyToil = mapM (verifyAndApplyTx False . withTxId)

-- | Apply transactions from one block. They must be valid (for
-- example, it implies topological sort).
applyToil
    :: GlobalTxpMode m
#ifdef WITH_EXPLORER
    => Timestamp
    -> [(TxAux, TxUndo)]
    -> HeaderHash
    -> m ()
applyToil curTime txun hh = do
#else
    => [(TxAux, TxUndo)]
    -> m ()
applyToil txun = do
#endif
    applyTxsToBalances txun
#ifdef WITH_EXPLORER
    mapM_ applier $ zip [0..] txun
  where
    applier (i, (txaux@(tx, _, _), txundo)) = do
        let id = hash tx
            newExtra = TxExtra (Just (hh, i)) curTime txundo
        extra <- maybe newExtra identity <$> getTxExtra id
        applyTxToUtxo' (id, txaux)
        putTxExtra id extra
#else
    mapM_ (applyTxToUtxo' . withTxId . fst) txun
#endif

-- | Rollback transactions from one block.
rollbackToil :: GlobalTxpMode m => [(TxAux, TxUndo)] -> m ()
rollbackToil txun = do
    rollbackTxsBalances txun
    mapM_ Utxo.rollbackTxUtxo $ reverse txun
#ifdef WITH_EXPLORER
    mapM_ (delTxExtra . hash . view _1 . fst) txun
#endif

----------------------------------------------------------------------------
-- Local
----------------------------------------------------------------------------

type LocalTxpMode m = ( MonadUtxo m
                      , MonadTxPool m
#ifdef WITH_EXPLORER
                      , MonadTxExtra m
#endif
                      )

-- CHECK: @processTx
-- #processWithPureChecks
-- | Verify one transaction and also add it to mem pool and apply to utxo
-- if transaction is valid.
#ifdef WITH_EXPLORER
processTx
    :: (LocalTxpMode m, MonadError ToilVerFailure m)
    => (TxId, TxAux) -> TxExtra -> m ()
processTx tx@(id, aux) extra = do
#else
processTx
    :: (LocalTxpMode m, MonadError ToilVerFailure m)
    => (TxId, TxAux) -> m ()
processTx tx@(id, aux) = do
#endif
    whenM (hasTx id) $ throwError ToilKnown
    whenM ((>= maxLocalTxs) <$> poolSize) $ throwError ToilOverwhelmed
    undo <- verifyAndApplyTx True tx
    putTxWithUndo id aux undo
#ifdef WITH_EXPLORER
    putTxExtra id extra
#endif

-- | Get rid of invalid transactions.
-- All valid transactions will be added to mem pool and applied to utxo.
normalizeToil
    :: (LocalTxpMode m)
#ifdef WITH_EXPLORER
    => [(TxId, (TxAux, TxExtra))]
#else
    => [(TxId, TxAux)]
#endif
    -> m ()
normalizeToil txs = mapM_ normalize ordered
  where
    ordered = fromMaybe txs $ topsortTxs wHash txs
#ifdef WITH_EXPLORER
    wHash (i, ((t, _, _), _)) = WithHash t i
    normalize = runExceptT . uncurry processTx . repair
    repair (i, (txaux, extra)) = ((i, txaux), extra)
#else
    wHash (i, (t, _, _)) = WithHash t i
    normalize = runExceptT . processTx
#endif

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
