{-# LANGUAGE TypeFamilies         #-}

-- | Type class necessary for Transaction processing (Txp)
-- and some useful getters and setters.

module Pos.Txp.MemState.Class
       ( MonadTxpMem (..)
       , getTxpLocalData
       , modifyTxpLocalData
       , setTxpLocalData
       , getUtxoView
       , getLocalTxsNUndo
       , getMemPool
       , getLocalTxs
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
import           Pos.Txp.Toil.Types     (MemPool (_mpLocalTxs), UtxoView)



-- | Reduced equivalent of @MonadReader TxpLDWrap m@.
class Monad m => MonadTxpMem m where
    askTxpMem :: m TxpLocalData
    -- ^ Retrieve 'TxpLDWrap'.

    -- | Default implementation for 'MonadTrans'.
    default askTxpMem
        :: (MonadTrans t, MonadTxpMem m', t m' ~ m) => m TxpLocalData
    askTxpMem = lift askTxpMem

instance MonadTxpMem m => MonadTxpMem (ReaderT s m)
instance MonadTxpMem m => MonadTxpMem (StateT s m)
instance MonadTxpMem m => MonadTxpMem (ExceptT s m)
instance MonadTxpMem m => MonadTxpMem (KademliaDHT m)

getTxpLocalData :: (MonadIO m, MonadTxpMem m) => m TxpLocalDataPure
getTxpLocalData = askTxpMem >>= \TxpLocalData{..} -> atomically $
    (,,,) <$> STM.readTVar txpUtxoView
          <*> STM.readTVar txpMemPool
          <*> STM.readTVar txpUndos
          <*> STM.readTVar txpTip

modifyTxpLocalData :: (MonadIO m, MonadTxpMem m) => (TxpLocalDataPure -> (a, TxpLocalDataPure)) -> m a
modifyTxpLocalData f =
    askTxpMem >>= \TxpLocalData{..} -> atomically $ do
        curUV  <- STM.readTVar txpUtxoView
        curMP  <- STM.readTVar txpMemPool
        curUndos <- STM.readTVar txpUndos
        curTip <- STM.readTVar txpTip
        let (res, (newUV, newMP, newUndos, newTip))
              = f (curUV, curMP, curUndos, curTip)
        STM.writeTVar txpUtxoView newUV
        STM.writeTVar txpMemPool newMP
        STM.writeTVar txpUndos newUndos
        STM.writeTVar txpTip newTip
        pure res

setTxpLocalData :: (MonadIO m, MonadTxpMem m) => TxpLocalDataPure -> m ()
setTxpLocalData = modifyTxpLocalData_ . const

modifyTxpLocalData_ :: (MonadIO m, MonadTxpMem m) => (TxpLocalDataPure -> TxpLocalDataPure) -> m ()
modifyTxpLocalData_ = modifyTxpLocalData . (((),) .)

getUtxoView :: (MonadTxpMem m, MonadIO m) => m UtxoView
getUtxoView = (\(uv, _, _, _) -> uv) <$> getTxpLocalData

getLocalTxs :: (MonadIO m, MonadTxpMem m) => m [(TxId, TxAux)]
getLocalTxs = HM.toList . _mpLocalTxs <$> getMemPool

getLocalTxsNUndo
    :: (MonadIO m, MonadTxpMem m)
    => m ([(TxId, TxAux)], HashMap TxId [TxOutAux])
getLocalTxsNUndo =
    (\(_, mp, undos, _) -> (HM.toList . _mpLocalTxs $ mp, undos)) <$> getTxpLocalData

getMemPool :: (MonadIO m, MonadTxpMem m) => m MemPool
getMemPool = (\(_, mp, _, _) -> mp) <$> getTxpLocalData
