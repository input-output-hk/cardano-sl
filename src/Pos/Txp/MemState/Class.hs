{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Type class necessary for Transaction processing (Txp)
-- and some useful getters and setters.

module Pos.Txp.MemState.Class
       ( MonadTxpMem
       , askTxpMem
       , TxpHolderTag
       , getUtxoModifier
       , getLocalTxsNUndo
       , getMemPool
       , getLocalTxs
       , getLocalTxsMap
       , getTxpExtra
       , modifyTxpLocalData
       , setTxpLocalData
       , clearTxpMemPool
       ) where

import           Universum

import qualified Control.Concurrent.STM as STM
import           Data.Default           (Default (def))
import qualified Data.HashMap.Strict    as HM
import qualified Ether

import           Pos.Txp.Core.Types     (TxAux, TxId, TxOutAux)
import           Pos.Txp.MemState.Types (GenericTxpLocalData (..),
                                         GenericTxpLocalDataPure)
import           Pos.Txp.Toil.Types     (MemPool (_mpLocalTxs), UtxoModifier)

data TxpHolderTag

-- | Reduced equivalent of @MonadReader (GenericTxpLocalData mw) m@.
type MonadTxpMem ext = Ether.MonadReader TxpHolderTag (GenericTxpLocalData ext)

askTxpMem :: MonadTxpMem ext m => m (GenericTxpLocalData ext)
askTxpMem = Ether.ask @TxpHolderTag

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

modifyTxpLocalData
    :: (MonadIO m, MonadTxpMem ext m)
    => (GenericTxpLocalDataPure ext -> (a, GenericTxpLocalDataPure ext)) -> m a
modifyTxpLocalData f =
    askTxpMem >>= \TxpLocalData{..} -> atomically $ do
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
        pure res

setTxpLocalData
    :: (MonadIO m, MonadTxpMem ext m)
    => GenericTxpLocalDataPure ext -> m ()
setTxpLocalData x = modifyTxpLocalData (const ((), x))

clearTxpMemPool :: (MonadIO m, MonadTxpMem ext m, Default ext) => m ()
clearTxpMemPool = modifyTxpLocalData clearF
  where
    clearF (_, _, _, tip, _) = ((), (mempty, def, mempty, tip, def))
