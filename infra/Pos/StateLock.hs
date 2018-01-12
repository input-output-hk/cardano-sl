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
       , newEmptyStateLock
       , newStateLock

       , StateLockMetrics (..)
       , ignoreStateLockMetrics

       , modifyStateLock
       , withStateLock
       , withStateLockNoMetrics
       ) where

import           Universum

import           Data.Time.Units (Microsecond)
import           Mockable (CurrentTime, Mockable, currentTime)
import           System.Wlog (LoggerNameBox, WithLogger, askLoggerName, usingLoggerName)

import           Pos.Core (HeaderHash)
import           Pos.Util.Concurrent (modifyMVar, withMVar)
import           Pos.Util.Concurrent.PriorityLock (Priority (..), PriorityLock, newPriorityLock,
                                                   withPriorityLock)
import           Pos.Util.Util (HasLens', lensOf)


-- | A simple wrapper over 'MVar' which stores 'HeaderHash' (our
-- current tip) and is taken whenever we want to update GState or
-- other data dependent on GState.
data StateLock = StateLock
    { slTip  :: !(MVar HeaderHash)
    , slLock :: !PriorityLock
    }

-- | Create empty (i. e. locked) 'StateLock'.
newEmptyStateLock :: MonadIO m => m StateLock
newEmptyStateLock = StateLock <$> newEmptyMVar <*> newPriorityLock

-- | Create a 'StateLock' with given tip.
newStateLock :: MonadIO m => HeaderHash -> m StateLock
newStateLock tip = StateLock <$> newMVar tip <*> newPriorityLock

-- | Effectful getters and setters for metrics related to the actions
-- which use 'StateLock'.
--
--   TODO this should not be fixed at IO, if being able to mock features
--   remains a goal. But we can't free it up right now because the current
--   mockable system doesn't work well with ether.
--
--   TODO: make it type class if we want to use other things we have
--   in application and to be more consistent with other code. @gromak
data StateLockMetrics = StateLockMetrics
    { -- | Called when a thread begins to wait to modify the mempool.
      --   Parameter is the reason for modifying the mempool.
      slmWait    :: !(String -> LoggerNameBox IO ())
      -- | Called when a thread is granted the lock on the
      --   mempool. The first parameter is the reason for modifying
      --   the mempool. The second one indicates how long it waited.
    , slmAcquire :: !(String -> Microsecond -> LoggerNameBox IO ())
      -- | Called when a thread is finished modifying the mempool and
      --   has released the lock. Parameters indicate the reason for
      --   modifying the mempool, time elapsed since acquiring the
      --   lock, and new mempool size.
    , slmRelease :: !(String -> Microsecond -> LoggerNameBox IO ())
    }

-- | A 'StateLockMetrics' that never does any writes. Use it if you
-- don't care about metrics.
ignoreStateLockMetrics :: StateLockMetrics
ignoreStateLockMetrics = StateLockMetrics
    { slmWait = const (pure ())
    , slmAcquire = const (const (pure ()))
    , slmRelease = const (const (pure ()))
    }

type MonadStateLockBase ctx m
     = ( MonadIO m
       , MonadMask m
       , MonadReader ctx m
       , HasLens' ctx StateLock
       )

type MonadStateLock ctx m
     = ( MonadStateLockBase ctx m
       , WithLogger m
       , Mockable CurrentTime m
       , HasLens' ctx StateLockMetrics
       )

-- | Run an action acquiring 'StateLock' lock. Argument of
-- action is an old tip, result is put as a new tip.
modifyStateLock ::
       MonadStateLock ctx m
    => Priority
    -> String
    -> (HeaderHash -> m (HeaderHash, a))
    -> m a
modifyStateLock = stateLockHelper modifyMVar

-- | Run an action acquiring 'StateLock' lock without modifying tip.
withStateLock ::
       MonadStateLock ctx m => Priority -> String -> (HeaderHash -> m a) -> m a
withStateLock = stateLockHelper withMVar

-- | Version of 'withStateLock' that does not gather metrics
withStateLockNoMetrics ::
       MonadStateLockBase ctx m => Priority -> (HeaderHash -> m a) -> m a
withStateLockNoMetrics prio action = do
    StateLock mvar prioLock <- view (lensOf @StateLock)
    withPriorityLock prioLock prio $ withMVar mvar action

stateLockHelper ::
       MonadStateLock ctx m
    => (MVar HeaderHash -> (HeaderHash -> m b) -> m a)
    -> Priority
    -> String
    -> (HeaderHash -> m b)
    -> m a
stateLockHelper doWithMVar prio reason action = do
    StateLock mvar prioLock <- view (lensOf @StateLock)
    StateLockMetrics {..} <- view (lensOf @StateLockMetrics)
    lname <- askLoggerName
    liftIO . usingLoggerName lname $ slmWait reason
    timeBeginWait <- currentTime
    withPriorityLock prioLock prio $ doWithMVar mvar $ \hh -> do
        timeEndWait <- currentTime
        liftIO . usingLoggerName lname $
            slmAcquire reason (timeEndWait - timeBeginWait)
        timeBeginModify <- currentTime
        res <- action hh
        timeEndModify <- currentTime
        liftIO . usingLoggerName lname $
            slmRelease reason (timeEndModify - timeBeginModify)
        pure res
