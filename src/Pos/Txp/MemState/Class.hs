{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Type class necessary for Transaction processing (Txp)
-- and some useful getters and setters.

module Pos.Txp.MemState.Class
       ( MonadTxpMem (..)
       , getUtxoModifier
       , getLocalTxsNUndo
       , getMemPool
       , getLocalTxs
       , getLocalTxsMap
       , getTxpExtra
       , modifyTxpLocalData
       , setTxpLocalData
       ) where

import qualified Control.Concurrent.STM as STM
import           Control.Monad.Except   (ExceptT)
import           Control.Monad.State    (StateT)
import           Control.Monad.Trans    (MonadTrans)
import qualified Data.HashMap.Strict    as HM
import           System.IO.Unsafe       (unsafePerformIO)
import           Universum

import           Pos.DHT.Real           (KademliaDHT)
import           Pos.Txp.Core.Types     (TxAux, TxId, TxOutAux)
import           Pos.Txp.MemState.Types (GenericTxpLocalData (..),
                                         GenericTxpLocalDataPure)
import           Pos.Txp.Toil.Types     (MemPool (..), UtxoModifier)

-- | Reduced equivalent of @MonadReader (GenericTxpLocalData mw) m@.
class Monad m => MonadTxpMem extra m | m -> extra where
    askTxpMem :: m (GenericTxpLocalData extra)
    -- ^ Retrieve 'GenericTxpLocalData'.

    -- | Default implementation for 'MonadTrans'.
    default askTxpMem :: (MonadTrans t, MonadTxpMem extra m', t m' ~ m) =>
        m (GenericTxpLocalData extra)
    askTxpMem = lift askTxpMem

instance MonadTxpMem x m => MonadTxpMem x (ReaderT s m)
instance MonadTxpMem x m => MonadTxpMem x (StateT s m)
instance MonadTxpMem x m => MonadTxpMem x (ExceptT s m)
instance MonadTxpMem x m => MonadTxpMem x (KademliaDHT m)

getTxpLocalData
    :: (MonadIO m, MonadTxpMem e m)
    => (GenericTxpLocalData e -> STM.STM a) -> m a
getTxpLocalData getter = askTxpMem >>= \ld -> atomically (getter ld)

getUtxoModifier
    :: (MonadTxpMem e m, MonadIO m)
    => m UtxoModifier
getUtxoModifier = getTxpLocalData (STM.readTVar . txpUtxoModifier)

getLocalTxsMap
    :: (MonadIO m, MonadTxpMem e m)
    => m (HashMap TxId TxAux)
getLocalTxsMap = _mpLocalTxs <$> getMemPool

getLocalTxs
    :: (MonadIO m, MonadTxpMem e m)
    => m [(TxId, TxAux)]
getLocalTxs = HM.toList <$> getLocalTxsMap

getLocalTxsNUndo
    :: (MonadIO m, MonadTxpMem e m)
    => m ([(TxId, TxAux)], HashMap TxId (NonEmpty TxOutAux))
getLocalTxsNUndo =
    getTxpLocalData $ \TxpLocalData {..} ->
        (,) <$> (HM.toList . _mpLocalTxs <$> STM.readTVar txpMemPool) <*>
        STM.readTVar txpUndos

getMemPool :: (MonadIO m, MonadTxpMem e m) => m MemPool
getMemPool = getTxpLocalData (STM.readTVar . txpMemPool)

getTxpExtra :: (MonadIO m, MonadTxpMem e m) => m e
getTxpExtra = getTxpLocalData (STM.readTVar . txpExtra)

txpLocalDataLock :: TVar Bool
txpLocalDataLock = unsafePerformIO $ STM.newTVarIO True
{-# NOINLINE txpLocalDataLock #-}

withLocalDataLock :: STM.STM a -> STM.STM a
withLocalDataLock m = do
  STM.readTVar txpLocalDataLock >>= \case
    False -> STM.retry
    True -> STM.writeTVar txpLocalDataLock False
  r <- m
  STM.writeTVar txpLocalDataLock True
  pure r

modifyTxpLocalData
    :: (MonadIO m, MonadTxpMem ext m)
    => (GenericTxpLocalDataPure ext -> (a, GenericTxpLocalDataPure ext)) -> m a
modifyTxpLocalData f =
    askTxpMem >>= \TxpLocalData{..} -> do
        (res, setGaugeIO) <- atomically . withLocalDataLock $ do
            curUM  <- STM.readTVar txpUtxoModifier
            curMP  <- STM.readTVar txpMemPool
            curUndos <- STM.readTVar txpUndos
            curTip <- STM.readTVar txpTip
            curExtra <- STM.readTVar txpExtra
            let (res, (newUM, newMP, newUndos, newTip, newExtra))
                  = f (curUM, curMP, curUndos, curTip, curExtra)
            STM.writeTVar txpUtxoModifier newUM
            STM.writeTVar txpMemPool newMP
            STM.writeTVar txpUndos newUndos
            STM.writeTVar txpTip newTip
            STM.writeTVar txpExtra newExtra
            setGauge <- STM.readTVar txpSetGauge
            pure (res, setGauge $ _mpLocalTxsSize newMP)
        liftIO setGaugeIO
        pure res

setTxpLocalData
    :: (MonadIO m, MonadTxpMem ext m)
    => GenericTxpLocalDataPure ext -> m ()
setTxpLocalData x = modifyTxpLocalData (const ((), x))
