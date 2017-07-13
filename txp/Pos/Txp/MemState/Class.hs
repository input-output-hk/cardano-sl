{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Type class necessary for Transaction processing (Txp)
-- and some useful getters and setters.

module Pos.Txp.MemState.Class
       ( MonadTxpMem
       , askTxpMem
       , askTxpMemAndMetrics
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

import qualified Control.Concurrent.STM      as STM
import qualified Control.Exception.Lifted    as L
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Data.Default                (Default (def))
import qualified Data.HashMap.Strict         as HM
import           Ether.Internal              (HasLens (..))
import           System.IO.Unsafe            (unsafePerformIO)
import           System.Wlog                 (WithLogger, getLoggerName, usingLoggerName)

import           Pos.Txp.Core.Types          (TxAux, TxId, TxOutAux)
import           Pos.Txp.MemState.Types      (GenericTxpLocalData (..),
                                              GenericTxpLocalDataPure, TxpMetrics (..))
import           Pos.Txp.Toil.Types          (MemPool (..), UtxoModifier)
import           Pos.Util.TimeWarp           (currentTime)

data TxpHolderTag

-- | More general version of @MonadReader (GenericTxpLocalData mw, TxpMetrics) m@.
type MonadTxpMem ext ctx m
     = ( MonadReader ctx m
       , HasLens TxpHolderTag ctx (GenericTxpLocalData ext, TxpMetrics)
       )

askTxpMem :: MonadTxpMem ext ctx m => m (GenericTxpLocalData ext)
askTxpMem = fst <$> view (lensOf @TxpHolderTag)

askTxpMemAndMetrics :: MonadTxpMem ext ctx m => m (GenericTxpLocalData ext, TxpMetrics)
askTxpMemAndMetrics = view (lensOf @TxpHolderTag)

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
    => m ([(TxId, TxAux)], HashMap TxId (NonEmpty TxOutAux))
getLocalTxsNUndo =
    getTxpLocalData $ \TxpLocalData {..} ->
        (,) <$> (HM.toList . _mpLocalTxs <$> STM.readTVar txpMemPool) <*>
        STM.readTVar txpUndos

getMemPool :: (MonadIO m, MonadTxpMem e ctx m) => m MemPool
getMemPool = getTxpLocalData (STM.readTVar . txpMemPool)

getTxpExtra :: (MonadIO m, MonadTxpMem e ctx m) => m e
getTxpExtra = getTxpLocalData (STM.readTVar . txpExtra)

txpLocalDataLock :: MVar ()
txpLocalDataLock = unsafePerformIO $ newMVar ()
{-# NOINLINE txpLocalDataLock #-}

modifyTxpLocalData
    :: (WithLogger m, MonadIO m, MonadBaseControl IO m, MonadTxpMem ext ctx m)
    => String
    -> (GenericTxpLocalDataPure ext -> (a, GenericTxpLocalDataPure ext))
    -> m a
modifyTxpLocalData reason f =
    askTxpMemAndMetrics >>= \(TxpLocalData{..}, TxpMetrics{..}) -> do
        lname <- getLoggerName
        liftIO . usingLoggerName lname $ txpMetricsWait reason
        timeBeginWait <- currentTime
        (res, logMetricsRelease) <- withLock $ do
            timeEndWait <- currentTime
            liftIO . usingLoggerName lname $
                txpMetricsAcquire (timeEndWait - timeBeginWait)
            timeBeginModify <- currentTime
            (res, newSize) <- atomically $ do
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
                pure (res, _mpSize newMP)
            timeEndModify <- currentTime
            let logMetricsRelease = liftIO . usingLoggerName lname $ do
                    txpMetricsRelease (timeEndModify - timeBeginModify) newSize
            pure (res, logMetricsRelease)
        logMetricsRelease
        pure res
 where
   withLock = L.bracket_ (takeMVar txpLocalDataLock) (putMVar txpLocalDataLock ())

setTxpLocalData ::
       (WithLogger m, MonadIO m, MonadBaseControl IO m, MonadTxpMem ext ctx m)
    => String
    -> GenericTxpLocalDataPure ext
    -> m ()
setTxpLocalData reason x = modifyTxpLocalData reason (const ((), x))

clearTxpMemPool ::
       ( WithLogger m
       , MonadIO m
       , MonadBaseControl IO m
       , MonadTxpMem ext ctx m
       , Default ext
       )
    => String
    -> m ()
clearTxpMemPool reason = modifyTxpLocalData reason clearF
  where
    clearF (_, _, _, tip, _) = ((), (mempty, def, mempty, tip, def))
