{-# LANGUAGE TypeFamilies #-}

-- | Type class necessary for Transaction processing (Txp)
-- and some useful getters and setters.

module Pos.Txp.MemState.Class
       ( MonadTxpMem
       , TxpHolderTag
       , withTxpLocalData
       , getUtxoModifier
       , getLocalUndos
       , getMemPool
       , getLocalTxs
       , getLocalTxsMap
       , getTxpExtra
       , getTxpTip
       , setTxpLocalData
       , clearTxpMemPool
       ) where

import           Universum

import qualified Control.Concurrent.STM as STM
import           Data.Default           (Default(..))
import qualified Data.HashMap.Strict    as HM
import           Ether.Internal         (HasLens (..))

import           Pos.Core.Types         (HeaderHash)
import           Pos.Txp.Core.Types     (TxAux, TxId)
import           Pos.Txp.MemState.Types (GenericTxpLocalData (..))
import           Pos.Txp.Toil.Types     (MemPool (..), UndoMap, UtxoModifier)

data TxpHolderTag

-- | More general version of @MonadReader (GenericTxpLocalData mw) m@.
type MonadTxpMem ext ctx m
     = ( MonadReader ctx m
       , HasLens TxpHolderTag ctx (GenericTxpLocalData ext)
       )

askTxpMem :: MonadTxpMem ext ctx m => m (GenericTxpLocalData ext)
askTxpMem = view (lensOf @TxpHolderTag)

-- | Operate with some or all of the TXP local data.
--
--   Since this function takes an STM action, it can be used to
--   read or modify the components.
withTxpLocalData
    :: (MonadIO m, MonadTxpMem e ctx m)
    => (GenericTxpLocalData e -> STM.STM a) -> m a
withTxpLocalData f = askTxpMem >>= \ld -> atomically (f ld)

-- | Read the UTXO modifier from the local TXP data.
getUtxoModifier
    :: GenericTxpLocalData e -> STM.STM UtxoModifier
getUtxoModifier = STM.readTVar . txpUtxoModifier

getLocalTxsMap
    :: GenericTxpLocalData e -> STM.STM (HashMap TxId TxAux)
getLocalTxsMap = fmap _mpLocalTxs . getMemPool

getLocalTxs
    :: GenericTxpLocalData e -> STM.STM [(TxId, TxAux)]
getLocalTxs = fmap HM.toList . getLocalTxsMap

getLocalUndos
    :: GenericTxpLocalData e -> STM.STM UndoMap
getLocalUndos = STM.readTVar . txpUndos

getMemPool
    :: GenericTxpLocalData e -> STM.STM MemPool
getMemPool = STM.readTVar . txpMemPool

getTxpTip
    :: GenericTxpLocalData e -> STM.STM HeaderHash
getTxpTip = STM.readTVar . txpTip

getTxpExtra
    :: GenericTxpLocalData e -> STM.STM e
getTxpExtra = STM.readTVar . txpExtra

-- | Helper function to set all components of the TxpLocalData.
setTxpLocalData
    :: GenericTxpLocalData e
    -> (UtxoModifier, MemPool, UndoMap, HeaderHash, e)
    -> STM.STM ()
setTxpLocalData txpData (um, mp, un, hh, e) = do
    STM.writeTVar (txpUtxoModifier txpData) um
    STM.writeTVar (txpMemPool txpData) mp
    STM.writeTVar (txpUndos txpData) un
    STM.writeTVar (txpTip txpData) hh
    STM.writeTVar (txpExtra txpData) e

-- | Clear everything in local data with the exception of the
--   header tip.
clearTxpMemPool
    :: Default e
    => GenericTxpLocalData e
    -> STM ()
clearTxpMemPool txpData = do
  tip <- getTxpTip txpData
  setTxpLocalData txpData (mempty, def, mempty, tip, def)
