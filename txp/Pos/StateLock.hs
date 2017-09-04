{-# LANGUAGE TypeApplications #-}
{-|
Module:      Pos.StateLock
Description: A lock on the local state of a node

Provides a lock that is to be taken while modifying, or requiring a
consistent view on, the local state (the database and mempool).

It collects metrics on how long a given action waits on the lock, and
how long the action takes.
-}
module Pos.StateLock
       ( Priority (..)
       , StateLock (..)
       , newStateLock
       , modifyStateLock
       , withStateLock
       , withStateLockNoMetrics
       ) where

import           Universum

import           Control.Monad.Catch              (MonadMask)
import           Mockable                         (CurrentTime, Mockable, currentTime)
import           System.Wlog                      (WithLogger, getLoggerName,
                                                   usingLoggerName)

import           Pos.Core                         (HeaderHash)
import           Pos.Txp.MemState                 (GenericTxpLocalData (..), MonadTxpMem,
                                                   TxpMetrics (..), askTxpMemAndMetrics)
import           Pos.Txp.Toil.Types               (MemPool (..))
import           Pos.Util.Concurrent              (modifyMVar, withMVar)
import           Pos.Util.Concurrent.PriorityLock (Priority (..), PriorityLock,
                                                   newPriorityLock, withPriorityLock)
import           Pos.Util.Util                    (HasLens', lensOf)


-- | A simple wrapper over 'MVar' which stores 'HeaderHash' (our
-- current tip) and is taken whenever we want to update GState or
-- other data dependent on GState.
data StateLock = StateLock
    { slTip  :: !(MVar HeaderHash)
    , slLock :: !PriorityLock
    }

newStateLock :: MonadIO m => m StateLock
newStateLock = StateLock <$> newEmptyMVar <*> newPriorityLock

-- | Run an action acquiring 'StateLock' lock. Argument of
-- action is an old tip, result is put as a new tip.
modifyStateLock ::
       ( MonadIO m
       , MonadMask m
       , WithLogger m
       , Mockable CurrentTime m
       , MonadTxpMem ext ctx m
       , MonadReader ctx m
       , HasLens' ctx StateLock
       )
    => Priority
    -> String
    -> (HeaderHash -> m (HeaderHash, a))
    -> m a
modifyStateLock = stateLockHelper modifyMVar

-- | Run an action acquiring 'StateLock' lock without modifying tip.
withStateLock ::
       ( MonadIO m
       , MonadMask m
       , WithLogger m
       , Mockable CurrentTime m
       , MonadTxpMem ext ctx m
       , MonadReader ctx m
       , HasLens' ctx StateLock
       )
    => Priority
    -> String
    -> (HeaderHash -> m a)
    -> m a
withStateLock = stateLockHelper withMVar

-- | Version of 'withStateLock' that does not gather metrics
withStateLockNoMetrics ::
       ( MonadIO m
       , MonadMask m
       , MonadReader ctx m
       , HasLens' ctx StateLock
       )
    => Priority
    -> (HeaderHash -> m a)
    -> m a
withStateLockNoMetrics prio action = do
    StateLock mvar prioLock <- view (lensOf @StateLock)
    withPriorityLock prioLock prio $ withMVar mvar action

stateLockHelper
    :: ( MonadIO m
       , MonadMask m
       , WithLogger m
       , Mockable CurrentTime m
       , MonadTxpMem ext ctx m
       , MonadReader ctx m
       , HasLens' ctx StateLock
       )
    => (MVar HeaderHash -> (HeaderHash -> m b) -> m a)
    -> Priority
    -> String
    -> (HeaderHash -> m b)
    -> m a
stateLockHelper doWithMVar prio reason action =
    askTxpMemAndMetrics >>= \(TxpLocalData{..}, TxpMetrics{..}) -> do
        StateLock mvar prioLock <- view (lensOf @StateLock)
        lname <- getLoggerName
        liftIO . usingLoggerName lname $ txpMetricsWait reason
        timeBeginWait <- currentTime
        withPriorityLock prioLock prio $ doWithMVar mvar $ \hh -> do
            timeEndWait <- currentTime
            liftIO . usingLoggerName lname $
                txpMetricsAcquire (timeEndWait - timeBeginWait)
            timeBeginModify <- currentTime
            res <- action hh
            timeEndModify <- currentTime
            newSize <- _mpSize <$> atomically (readTVar txpMemPool)
            liftIO . usingLoggerName lname $
                txpMetricsRelease (timeEndModify - timeBeginModify) newSize
            pure res
