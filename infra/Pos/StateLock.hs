{-# LANGUAGE RankNTypes       #-}
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

import           Control.Monad.Catch (MonadMask)
import           Data.Aeson.Types (ToJSON (..), Value)
import           Data.Time.Units (Microsecond)
import           JsonLog (CanJsonLog (..))
import           Mockable (CurrentTime, Mockable, currentTime)
import           System.Mem (getAllocationCounter)
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
data StateLockMetrics slr = StateLockMetrics
    { -- | Called when a thread begins to wait to modify the mempool.
      --   Parameter is the reason for modifying the mempool.
      slmWait    :: !(slr -> LoggerNameBox IO ())
      -- | Called when a thread is granted the lock on the mempool. Parameter
      --   indicates how long it waited.
    , slmAcquire :: !(slr -> Microsecond -> LoggerNameBox IO ())
      -- | Called when a thread is finished modifying the mempool and has
      --   released the lock. Parameters indicates time elapsed since acquiring
      --   the lock, and new mempool size.
    , slmRelease :: !(slr -> Microsecond -> Microsecond -> Int64 -> LoggerNameBox IO Value)
    }

-- | A 'StateLockMetrics' that never does any writes. Use it if you
-- don't care about metrics.
ignoreStateLockMetrics :: StateLockMetrics ()
ignoreStateLockMetrics = StateLockMetrics
    { slmWait = \_ -> pure ()
    , slmAcquire = \_ _ -> pure ()
    , slmRelease = \_ _ _ _ -> pure (toJSON ())
    }

type MonadStateLockBase ctx m
     = ( MonadIO m
       , MonadMask m
       , MonadReader ctx m
       , HasLens' ctx StateLock
       )

type MonadStateLock ctx slr m
     = ( MonadStateLockBase ctx m
       , WithLogger m
       , Mockable CurrentTime m
       , HasLens' ctx (StateLockMetrics slr)
       , CanJsonLog m
       )

-- | Run an action acquiring 'StateLock' lock. Argument of
-- action is an old tip, result is put as a new tip.
modifyStateLock :: forall ctx slr m a.
       MonadStateLock ctx slr m
    => Priority
    -> slr
    -> (HeaderHash -> m (HeaderHash, a))
    -> m a
modifyStateLock = stateLockHelper modifyMVar

-- | Run an action acquiring 'StateLock' lock without modifying tip.
withStateLock ::
       MonadStateLock ctx slr m => Priority -> slr -> (HeaderHash -> m a) -> m a
withStateLock = stateLockHelper withMVar

-- | Version of 'withStateLock' that does not gather metrics
withStateLockNoMetrics ::
       MonadStateLockBase ctx m => Priority -> (HeaderHash -> m a) -> m a
withStateLockNoMetrics prio action = do
    StateLock mvar prioLock <- view (lensOf @StateLock)
    withPriorityLock prioLock prio $ withMVar mvar action

stateLockHelper :: forall ctx slr m a b.
       MonadStateLock ctx slr m
    => (MVar HeaderHash -> (HeaderHash -> m b) -> m a)
    -> Priority
    -> slr
    -> (HeaderHash -> m b)
    -> m a
stateLockHelper doWithMVar prio reason action = do
    StateLock mvar prioLock <- view (lensOf @StateLock)
    StateLockMetrics {..} <- view (lensOf @(StateLockMetrics slr))
    lname <- askLoggerName
    liftIO . usingLoggerName lname $ slmWait reason
    timeBeginWait <- currentTime
    withPriorityLock prioLock prio $ doWithMVar mvar $ \hh -> do
        timeEndWait <- currentTime
        liftIO . usingLoggerName lname $
            slmAcquire reason (timeEndWait - timeBeginWait)
        timeBeginModify <- currentTime
        memBeginModify <- liftIO getAllocationCounter
        res <- action hh
        timeEndModify <- currentTime
        memEndModify <- liftIO getAllocationCounter
        json <- liftIO . usingLoggerName lname $ slmRelease
            reason
            (timeEndWait - timeBeginWait)
            (timeEndModify - timeBeginModify)
            -- counter counts "down" memory that has been allocated by the thread
            (memBeginModify - memEndModify)
        jsonLog json
        pure res
