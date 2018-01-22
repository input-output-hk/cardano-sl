{-# LANGUAGE TypeFamilies #-}

-- | Type class necessary for transaction processing (Txp)
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

       , MonadTxpLocal (..)
       , TxpLocalWorkMode
       , MempoolExt
       ) where

import           Universum

import qualified Control.Concurrent.STM as STM
import           Control.Monad.Morph (generalize, hoist)
import           Data.Default (Default (def))
import qualified Data.HashMap.Strict as HM
import           Ether.Internal (HasLens (..))
import           Mockable (CurrentTime, Mockable)
import           System.Wlog (NamedPureLogger, WithLogger, launchNamedPureLog)

import           Pos.Core (HasConfiguration)
import           Pos.DB.Class (MonadDBRead, MonadGState (..))
import           Pos.Reporting (MonadReporting)
import           Pos.Slotting (MonadSlots (..))

import           Pos.Core.Txp (TxAux, TxId, TxUndo)
import           Pos.Txp.MemState.Types (GenericTxpLocalData (..), GenericTxpLocalDataPure)
import           Pos.Txp.Toil.Failure (ToilVerFailure)
import           Pos.Txp.Toil.Types (MemPool (..), UtxoModifier)

data TxpHolderTag

-- | More general version of @MonadReader (GenericTxpLocalData mw) m@.
type MonadTxpMem ext ctx m
     = ( MonadReader ctx m
       , HasLens TxpHolderTag ctx (GenericTxpLocalData ext)
       , Default ext
       )

askTxpMem :: MonadTxpMem ext ctx m => m (GenericTxpLocalData ext)
askTxpMem = view (lensOf @TxpHolderTag)

getTxpLocalData
    :: (MonadIO m, MonadTxpMem e ctx m)
    => (GenericTxpLocalData e -> STM.STM a) -> m a
getTxpLocalData getter = askTxpMem >>= \ld -> atomically (getter ld)

getUtxoModifier
    :: (MonadTxpMem e ctx m, MonadIO m)
    => m UtxoModifier
getUtxoModifier = getTxpLocalData (STM.readTVar . txpUtxoModifier)

getLocalTxsMap
    :: (MonadIO m, MonadTxpMem e ctx m)
    => m (HashMap TxId TxAux)
getLocalTxsMap = _mpLocalTxs <$> getMemPool

getLocalTxs
    :: (MonadIO m, MonadTxpMem e ctx m)
    => m [(TxId, TxAux)]
getLocalTxs = HM.toList <$> getLocalTxsMap

getLocalTxsNUndo
    :: (MonadIO m, MonadTxpMem e ctx m)
    => m ([(TxId, TxAux)], HashMap TxId TxUndo)
getLocalTxsNUndo =
    getTxpLocalData $ \TxpLocalData {..} ->
        (,) <$>
        (HM.toList . _mpLocalTxs <$> STM.readTVar txpMemPool) <*>
        STM.readTVar txpUndos

getMemPool :: (MonadIO m, MonadTxpMem e ctx m) => m MemPool
getMemPool = getTxpLocalData (STM.readTVar . txpMemPool)

getTxpExtra :: (MonadIO m, MonadTxpMem e ctx m) => m e
getTxpExtra = getTxpLocalData (STM.readTVar . txpExtra)

modifyTxpLocalData
    :: (MonadIO m, MonadTxpMem ext ctx m, WithLogger m)
    => (GenericTxpLocalDataPure ext -> NamedPureLogger Identity (a, GenericTxpLocalDataPure ext))
    -> m a
modifyTxpLocalData f =
    askTxpMem >>= \TxpLocalData{..} -> launchNamedPureLog atomically $ do
        curUM    <- lift $ STM.readTVar txpUtxoModifier
        curMP    <- lift $ STM.readTVar txpMemPool
        curUndos <- lift $ STM.readTVar txpUndos
        curTip   <- lift $ STM.readTVar txpTip
        curExtra <- lift $ STM.readTVar txpExtra
        (res, (newUM, newMP, newUndos, newTip, newExtra)) <- hoist generalize $
                f (curUM, curMP, curUndos, curTip, curExtra)
        lift $ STM.writeTVar txpUtxoModifier newUM
        lift $ STM.writeTVar txpMemPool newMP
        lift $ STM.writeTVar txpUndos newUndos
        lift $ STM.writeTVar txpTip newTip
        lift $ STM.writeTVar txpExtra newExtra
        pure res

setTxpLocalData ::
       (MonadIO m, MonadTxpMem ext ctx m, WithLogger m)
    => GenericTxpLocalDataPure ext
    -> m ()
setTxpLocalData x = modifyTxpLocalData (const $ pure ((), x))

clearTxpMemPool ::
       ( MonadIO m
       , MonadTxpMem ext ctx m
       , Default ext
       , WithLogger m
       )
    => m ()
clearTxpMemPool = modifyTxpLocalData clearF
  where
    clearF (_, _, _, tip, _) = pure ((), (mempty, def, mempty, tip, def))

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
    , HasConfiguration
    )
