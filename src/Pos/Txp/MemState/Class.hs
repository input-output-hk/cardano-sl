{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

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

import qualified Control.Concurrent.STM as STM
import qualified Control.Monad.Ether    as Ether.E
import           Data.Default           (Default (..))
import qualified Data.HashMap.Strict    as HM
import           Data.IORef             (IORef)
import           System.IO.Unsafe       (unsafePerformIO)
import           System.Wlog            (WithLogger, logDebug, usingLoggerName,
                                         getLoggerName)
import           Formatting             (sformat, (%), shown)
import           Universum
import qualified GHC.Conc               as Conc

import           Pos.Txp.Core.Types     (TxAux, TxId, TxOutAux)
import           Pos.Txp.MemState.Types (GenericTxpLocalData (..),
                                         GenericTxpLocalDataPure,
                                         TxpMetrics (..), MemPoolModifyReason)
import           Pos.Txp.Toil.Types     (MemPool (..), UtxoModifier)
import           Pos.Util.TimeWarp      (currentTime)
import           Pos.Util.JsonLog       (MonadJL, jlLog, JLMemPool(..),
                                         JLEvent(JLMemPoolEvent))

data TxpHolderTag

-- | Reduced equivalent of @MonadReader (GenericTxpLocalData mw) m@.
type MonadTxpMem ext m =
    ( Ether.E.MonadReader TxpHolderTag (GenericTxpLocalData ext, TxpMetrics) m
    , MonadJL m
    , WithLogger m
    )

askTxpMem :: MonadTxpMem ext m => m (GenericTxpLocalData ext)
askTxpMem = fst <$> Ether.E.ask (Proxy @TxpHolderTag)

askTxpMemAndMetrics :: MonadTxpMem ext m => m (GenericTxpLocalData ext, TxpMetrics)
askTxpMemAndMetrics = Ether.E.ask (Proxy @TxpHolderTag)

getTxpLocalData
    :: (MonadIO m, MonadTxpMem e m)
    => (GenericTxpLocalData e -> STM.STM a) -> m a
getTxpLocalData getter = askTxpMem >>= \ld -> do
    beginTime <- currentTime
    res <- atomically (getter ld)
    endTime <- currentTime
    let duration = endTime - beginTime
    logDebug $ sformat ("getTxpLocalData took " % shown) duration
    return res

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

txpLocalDataLock :: MVar ()
txpLocalDataLock = unsafePerformIO $ newMVar ()
{-# NOINLINE txpLocalDataLock #-}

-- | An IORef to hold the current number of threads waiting on the
--   txpLocalDataLock. It's incremented before taking it, and decremented
--   after it's taken, so it's not necessarily exact.
txpLocalDataLockQueueLength :: IORef Int
txpLocalDataLockQueueLength = unsafePerformIO $ newIORef 0
{-# NOINLINE txpLocalDataLockQueueLength #-}

modifyTxpLocalData
    :: (MonadIO m, MonadTxpMem ext m, WithLogger m)
    => MemPoolModifyReason
    -> (GenericTxpLocalDataPure ext -> (a, GenericTxpLocalDataPure ext))
    -> m a
modifyTxpLocalData reason f =
    askTxpMemAndMetrics >>= \(TxpLocalData{..}, TxpMetrics{..}) -> do
        lname <- getLoggerName
        liftIO . usingLoggerName lname $ txpMetricsWait reason
        qlength <- atomicModifyIORef' txpLocalDataLockQueueLength $ \i -> (i + 1, i)
        timeBeginWait <- currentTime
        _ <- takeMVar txpLocalDataLock
        timeEndWait <- currentTime
        _ <- atomicModifyIORef' txpLocalDataLockQueueLength $ \i -> (i - 1, ())
        let timeWait = timeEndWait - timeBeginWait
        liftIO . usingLoggerName lname $ txpMetricsAcquire timeWait
        allocBeginModify <- liftIO Conc.getAllocationCounter
        timeBeginModify <- currentTime
        (res, oldSize, newSize) <- atomically $ do
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
            pure (res, _mpLocalTxsSize curMP, _mpLocalTxsSize newMP)
        timeEndModify <- currentTime
        allocEndModify <- liftIO Conc.getAllocationCounter
        putMVar txpLocalDataLock ()
        let timeModify = timeEndModify - timeBeginModify
            -- Allocation counter counts down, so
            -- allocBeginModify >= allocEndModify
            allocModify = allocBeginModify - allocEndModify
        liftIO . usingLoggerName lname $ txpMetricsRelease (timeEndModify - timeBeginModify) newSize
        let jsonEvent = JLMemPoolEvent $ JLMemPool
                { jlmReason      = reason
                , jlmQueueLength = qlength
                  -- Wait and modify times in the JLMemPool datatype are
                  -- not Microseconds, because Microseconds has no aeson
                  -- instances.
                , jlmWait        = fromIntegral timeWait
                , jlmModify      = fromIntegral timeModify
                , jlmSizeBefore  = oldSize
                , jlmSizeAfter   = newSize
                  -- fromIntegral :: Int64 -> Int
                  -- It's probably fine.
                , jlmAllocated   = fromIntegral allocModify
                }
        jlLog jsonEvent
        pure res

setTxpLocalData
    :: (MonadIO m, MonadTxpMem ext m)
    => MemPoolModifyReason
    -> GenericTxpLocalDataPure ext
    -> m ()
setTxpLocalData reason x = modifyTxpLocalData reason (const ((), x))

clearTxpMemPool
    :: (MonadIO m, MonadTxpMem ext m, Default ext)
    => MemPoolModifyReason
    -> m ()
clearTxpMemPool reason = modifyTxpLocalData reason clearF
  where
    clearF (_, _, _, tip, _) = ((), (mempty, def, mempty, tip, def))
