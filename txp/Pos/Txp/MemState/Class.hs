{-# LANGUAGE TypeFamilies #-}

-- | Type class necessary for transaction processing (Txp)
-- and some useful getters and setters.

module Pos.Txp.MemState.Class
       ( MonadTxpMem
       , TxpHolderTag
       , withTxpLocalData
       , withTxpLocalDataLog
       , getUtxoModifier
       , getLocalUndos
       , getMemPool
       , getLocalTxs
       , getLocalTxsMap
       , getTxpExtra
       , getTxpTip
       , setTxpLocalData
       , clearTxpMemPool

       , MonadTxpLocal (..)
       , TxpLocalWorkMode
       , MempoolExt
       ) where

import           Universum

import qualified Control.Concurrent.STM as STM
import           Data.Default (Default (..))
import qualified Data.HashMap.Strict as HM
import           Mockable (CurrentTime, Mockable)
import           Pos.Core.Common (HeaderHash)
import           Pos.Core.Txp (TxAux, TxId)
import           Pos.DB.Class (MonadDBRead, MonadGState (..))
import           Pos.Reporting (MonadReporting)
import           Pos.Slotting (MonadSlots (..))
import           Pos.Txp.Configuration (HasTxpConfiguration)
import           Pos.Txp.MemState.Types (GenericTxpLocalData (..))
import           Pos.Txp.Toil.Failure (ToilVerFailure)
import           Pos.Txp.Toil.Types (MemPool (..), UndoMap, UtxoModifier)
import           Pos.Util.Util (HasLens (..))
import           System.Wlog (NamedPureLogger, WithLogger, launchNamedPureLog)

data TxpHolderTag

-- | More general version of @MonadReader (GenericTxpLocalData mw) m@.
type MonadTxpMem ext ctx m
     = ( MonadReader ctx m
       , HasLens TxpHolderTag ctx (GenericTxpLocalData ext)
       , Default ext
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

-- | Operate with some of all of the TXP local data, allowing
--   logging.
withTxpLocalDataLog
    :: (MonadIO m, MonadTxpMem e ctx m, WithLogger m)
    => (GenericTxpLocalData e -> NamedPureLogger STM.STM a)
    -> m a
withTxpLocalDataLog f = askTxpMem >>=
    \ld -> launchNamedPureLog atomically $ f ld

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

----------------------------------------------------------------------------
-- Abstract txNormalize and processTx
----------------------------------------------------------------------------

type family MempoolExt (m :: * -> *) :: *

class Monad m => MonadTxpLocal m where
    txpNormalize :: m ()
    txpProcessTx :: (TxId, TxAux) -> m (Either ToilVerFailure ())

type TxpLocalWorkMode ctx m =
    ( MonadIO m
    , MonadDBRead m
    , MonadGState m
    , MonadSlots ctx m
    , MonadTxpMem (MempoolExt m) ctx m
    , WithLogger m
    , Mockable CurrentTime m
    , MonadMask m
    , MonadReporting ctx m
    , HasTxpConfiguration
    )
