{-# LANGUAGE TypeFamilies #-}

-- | Type class necessary for Transaction processing (Txp)
-- and some useful getters and setters.

module Pos.Txp.MemState.Class
       ( MonadTxpMem (..)
       , getUtxoModifier
       , getLocalTxsNUndo
       , getMemPool
       , getLocalTxs
       , modifyTxpLocalData
       , setTxpLocalData
       ) where

import qualified Control.Concurrent.STM as STM
import           Control.Monad.Except   (ExceptT)
import           Control.Monad.State    (StateT)
import           Control.Monad.Trans    (MonadTrans)
import qualified Data.HashMap.Strict    as HM
import           Universum

import           Pos.DHT.Real           (KademliaDHT)
import           Pos.Txp.Core.Types     (TxAux, TxId, TxOutAux)
import           Pos.Txp.MemState.Types (TxpLocalData (..), TxpLocalDataPure)
import           Pos.Txp.Toil.Types     (MemPool (_mpLocalTxs), UtxoModifier)

-- | Reduced equivalent of @MonadReader TxpLocalData m@.
class Monad m => MonadTxpMem m where
    askTxpMem :: m TxpLocalData
    -- ^ Retrieve 'TxpLocalData'.

    -- | Default implementation for 'MonadTrans'.
    default askTxpMem
        :: (MonadTrans t, MonadTxpMem m', t m' ~ m) => m TxpLocalData
    askTxpMem = lift askTxpMem

instance MonadTxpMem m => MonadTxpMem (ReaderT s m)
instance MonadTxpMem m => MonadTxpMem (StateT s m)
instance MonadTxpMem m => MonadTxpMem (ExceptT s m)
instance MonadTxpMem m => MonadTxpMem (KademliaDHT m)

getTxpLocalData
    :: (MonadIO m, MonadTxpMem m)
    => (TxpLocalData -> STM.STM a) -> m a
getTxpLocalData getter = askTxpMem >>= \ld -> atomically (getter ld)

getUtxoModifier :: (MonadTxpMem m, MonadIO m) => m UtxoModifier
getUtxoModifier = getTxpLocalData (STM.readTVar . txpUtxoModifier)

getLocalTxs :: (MonadIO m, MonadTxpMem m) => m [(TxId, TxAux)]
getLocalTxs = HM.toList . _mpLocalTxs <$> getMemPool

getLocalTxsNUndo
    :: (MonadIO m, MonadTxpMem m)
    => m ([(TxId, TxAux)], HashMap TxId (NonEmpty TxOutAux))
getLocalTxsNUndo =
    getTxpLocalData $ \TxpLocalData {..} ->
        (,) <$> (HM.toList . _mpLocalTxs <$> STM.readTVar txpMemPool) <*>
        STM.readTVar txpUndos

getMemPool :: (MonadIO m, MonadTxpMem m) => m MemPool
getMemPool = getTxpLocalData (STM.readTVar . txpMemPool)

modifyTxpLocalData :: (MonadIO m, MonadTxpMem m) => (TxpLocalDataPure -> (a, TxpLocalDataPure)) -> m a
modifyTxpLocalData f =
    askTxpMem >>= \TxpLocalData{..} -> atomically $ do
        curUM  <- STM.readTVar txpUtxoModifier
        curMP  <- STM.readTVar txpMemPool
        curUndos <- STM.readTVar txpUndos
        curTip <- STM.readTVar txpTip
        let (res, (newUM, newMP, newUndos, newTip))
              = f (curUM, curMP, curUndos, curTip)
        STM.writeTVar txpUtxoModifier newUM
        STM.writeTVar txpMemPool newMP
        STM.writeTVar txpUndos newUndos
        STM.writeTVar txpTip newTip
        pure res

setTxpLocalData :: (MonadIO m, MonadTxpMem m) => TxpLocalDataPure -> m ()
setTxpLocalData = modifyTxpLocalData_ . const

modifyTxpLocalData_ :: (MonadIO m, MonadTxpMem m) => (TxpLocalDataPure -> TxpLocalDataPure) -> m ()
modifyTxpLocalData_ = modifyTxpLocalData . (((),) .)
