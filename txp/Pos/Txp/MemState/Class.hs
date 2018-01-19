{-# LANGUAGE TypeFamilies #-}

-- | Type class necessary for Transaction processing (Txp)
-- and some useful getters and setters.

module Pos.Txp.MemState.Class
       ( MonadTxpMem
       , askTxpMem
       , TxpHolderTag
       , getUtxoModifier
       , getLocalTxsNUndo
       , getMemPoolSnapshot
       , getLocalTxs
       , getLocalTxsMap
       , getTxpExtra
       , modifyTxpLocalData
       , setTxpLocalData
       , clearTxpMemPool
       , MemPoolSnapshot
       ) where

import           Universum

import qualified Control.Concurrent.STM as STM
import           Data.Default           (Default (def))
import qualified Data.HashMap.Strict    as HM
import           Ether.Internal         (HasLens (..))

import           Pos.Txp.Core.Types     (TxAux, TxId, TxUndo)
import           Pos.Txp.MemState.Types (GenericTxpLocalData (..),
                                         GenericTxpLocalDataPure)
import           Pos.Txp.Toil.Types     (MemPool (..), UndoMap, UtxoModifier)

data TxpHolderTag

-- | More general version of @MonadReader (GenericTxpLocalData mw) m@.
type MonadTxpMem ext ctx m
     = ( MonadReader ctx m
       , HasLens TxpHolderTag ctx (GenericTxpLocalData ext)
       )

-- A 'MemPoolSnapshot' bundles together 3 key piece of information which will otherwise go out of sync
-- if fetched at separated times, due to the fact they are stored in a separate TVar each.
data MemPoolSnapshot = MemPoolSnapshot {
      mpsMemPool      :: MemPool
    , mpsUndoMap      :: UndoMap
    , mpsUtxoModifier :: UtxoModifier
    }

askTxpMem :: MonadTxpMem ext ctx m => m (GenericTxpLocalData ext)
askTxpMem = view (lensOf @TxpHolderTag)

getTxpLocalData
    :: (MonadIO m, MonadTxpMem e ctx m)
    => (GenericTxpLocalData e -> STM.STM a) -> m a
getTxpLocalData getter = askTxpMem >>= \ld -> atomically (getter ld)

getUtxoModifier :: MemPoolSnapshot -> UtxoModifier
getUtxoModifier MemPoolSnapshot{..} = mpsUtxoModifier

getLocalTxsMap :: MemPoolSnapshot -> HashMap TxId TxAux
getLocalTxsMap = _mpLocalTxs . mpsMemPool

getLocalTxs :: MemPoolSnapshot -> [(TxId, TxAux)]
getLocalTxs = HM.toList . getLocalTxsMap

getLocalTxsNUndo :: MemPoolSnapshot -> ([(TxId, TxAux)], HashMap TxId TxUndo)
getLocalTxsNUndo MemPoolSnapshot{..} = (HM.toList $ _mpLocalTxs mpsMemPool, mpsUndoMap)

-- | Reads the 'MemPoolSnapshot'. This is the only function which we allow to fetch the
-- 'MemPool' directly. All the other are pure functions which accepts a 'MemPoolSnapshot' as input.
getMemPoolSnapshot :: (MonadIO m, MonadTxpMem e ctx m) => m MemPoolSnapshot
getMemPoolSnapshot = getTxpLocalData (\gld -> MemPoolSnapshot <$> STM.readTVar (txpMemPool gld)
                                                              <*> STM.readTVar (txpUndos gld)
                                                              <*> STM.readTVar (txpUtxoModifier gld)
                                     )

getTxpExtra :: (MonadIO m, MonadTxpMem e ctx m) => m e
getTxpExtra = getTxpLocalData (STM.readTVar . txpExtra)

modifyTxpLocalData
    :: (MonadIO m, MonadTxpMem ext ctx m)
    => (GenericTxpLocalDataPure ext -> (a, GenericTxpLocalDataPure ext))
    -> m a
modifyTxpLocalData f =
    askTxpMem >>= \TxpLocalData{..} -> atomically $ do
        curUM <- STM.readTVar txpUtxoModifier
        curMP <- STM.readTVar txpMemPool
        curUndos <- STM.readTVar txpUndos
        curTip <- STM.readTVar txpTip
        curExtra <- STM.readTVar txpExtra
        let (res,(newUM,newMP,newUndos,newTip,newExtra)) =
                f (curUM, curMP, curUndos, curTip, curExtra)
        STM.writeTVar txpUtxoModifier newUM
        STM.writeTVar txpMemPool newMP
        STM.writeTVar txpUndos newUndos
        STM.writeTVar txpTip newTip
        STM.writeTVar txpExtra newExtra
        pure res

setTxpLocalData ::
       (MonadIO m, MonadTxpMem ext ctx m)
    => GenericTxpLocalDataPure ext
    -> m ()
setTxpLocalData x = modifyTxpLocalData (const ((), x))

clearTxpMemPool ::
       ( MonadIO m
       , MonadTxpMem ext ctx m
       , Default ext
       )
    => m ()
clearTxpMemPool = modifyTxpLocalData clearF
  where
    clearF (_, _, _, tip, _) = ((), (mempty, def, mempty, tip, def))
