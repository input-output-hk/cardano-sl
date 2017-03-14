{-# LANGUAGE CPP             #-}
{-# LANGUAGE ConstraintKinds #-}
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
import           Pos.Binary.Class      (biSize)

import           Pos.Txp.Core          (TxAux, TxId, TxUndo, TxpUndo, topsortTxs)
import           Pos.Txp.Toil.Balances (applyTxsToBalances, rollbackTxsBalances)
import           Pos.Txp.Toil.Class    (MonadBalances (..), MonadTxPool (..),
                                        MonadToilEnv (..), MonadUtxo (..))
import           Pos.Txp.Toil.Failure  (ToilVerFailure (..))
import           Pos.Txp.Toil.Types    (ToilEnv (teMaxTxSize))
import qualified Pos.Txp.Toil.Utxo     as Utxo
#ifdef WITH_EXPLORER
import           Data.List             (delete, union)
import qualified Data.List.NonEmpty    as NE
import           Pos.Txp.Core          (Tx (..), TxOut (..), TxOutAux (..))
import           Pos.Txp.Toil.Class    (MonadTxExtra (..), MonadTxExtraRead (..))
import           Pos.Types             (AddrHistory, Address, HeaderHash, Timestamp,
                                        TxExtra (..))
import           Pos.Util              (NewestFirst (..))
#endif

----------------------------------------------------------------------------
-- Global
----------------------------------------------------------------------------

type GlobalTxpMode m = ( MonadUtxo m
                       , MonadBalances m
                       , MonadToilEnv m
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
        putTxExtraWithHistory id extra $ getTxRelatedAddrs txaux txundo
#else
    mapM_ (applyTxToUtxo' . withTxId . fst) txun
#endif

-- | Rollback transactions from one block.
rollbackToil :: GlobalTxpMode m => [(TxAux, TxUndo)] -> m ()
rollbackToil txun = do
    rollbackTxsBalances txun
    mapM_ Utxo.rollbackTxUtxo $ reverse txun
#ifdef WITH_EXPLORER
    mapM_ extraRollback txun
  where
    extraRollback (txaux@(tx, _, _), txundo) =
        delTxExtraWithHistory (hash tx) $ getTxRelatedAddrs txaux txundo
#endif

----------------------------------------------------------------------------
-- Local
----------------------------------------------------------------------------

type LocalTxpMode m = ( MonadUtxo m
                      , MonadToilEnv m
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
    putTxExtraWithHistory id extra $ getTxRelatedAddrs aux undo
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

#ifdef WITH_EXPLORER
modifyAddrHistory
    :: MonadTxExtra m
    => (AddrHistory -> AddrHistory)
    -> Address
    -> m ()
modifyAddrHistory f addr =
    updateAddrHistory addr . f =<< getAddrHistory addr

putTxExtraWithHistory
    :: MonadTxExtra m
    => TxId
    -> TxExtra
    -> NonEmpty Address
    -> m ()
putTxExtraWithHistory id extra addrs = do
    putTxExtra id extra
    forM_ addrs $ modifyAddrHistory $
        NewestFirst . (id :) . getNewestFirst

delTxExtraWithHistory
    :: MonadTxExtra m
    => TxId
    -> NonEmpty Address
    -> m ()
delTxExtraWithHistory id addrs = do
    delTxExtra id
    forM_ addrs $ modifyAddrHistory $
        NewestFirst . delete id . getNewestFirst

getTxRelatedAddrs :: TxAux -> TxUndo -> NonEmpty Address
getTxRelatedAddrs (UnsafeTx {..}, _, _) undo = NE.fromList $
    map txOutAddress (NE.toList _txOutputs) `union`
    map (txOutAddress . toaOut) (NE.toList undo)
#endif
