{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Mockable.Concurrent (

    ThreadId
  , Fork(..)
  , fork
  , myThreadId
  , killThread

  , Delay(..)
  , wait
  , SleepForever(..)
  , sleepForever
  , RepeatForever(..)
  , repeatForever

  , RunInUnboundThread(..)
  , runInUnboundThread

  ) where

import Mockable.Class

import Data.Time.Units          (Microsecond)
import Control.Exception.Base   (SomeException)

type family ThreadId (m :: * -> *) :: *

data Fork m t where
    Fork :: m () -> Fork m (ThreadId m)
    MyThreadId :: Fork m (ThreadId m)
    KillThread :: ThreadId m -> Fork m ()

fork :: ( Mockable Fork m ) => m () -> m (ThreadId m)
fork term = liftMockable $ Fork term

myThreadId :: ( Mockable Fork m ) => m (ThreadId m)
myThreadId = liftMockable MyThreadId

killThread :: ( Mockable Fork m ) => ThreadId m -> m ()
killThread tid = liftMockable $ KillThread tid

-- | Defines some time point basing on current time.
type RelativeToNow = Microsecond -> Microsecond

data Delay (m :: * -> *) (t :: *) where
    Delay :: RelativeToNow -> Delay m ()

wait :: ( Mockable Delay m ) => RelativeToNow -> m ()
wait relativeToNow = liftMockable $ Delay relativeToNow

data SleepForever (m :: * -> *) (t :: *) where
    SleepForever :: SleepForever m ()

sleepForever :: ( Mockable SleepForever m ) => m ()
sleepForever = liftMockable SleepForever

data RepeatForever (m :: * -> *) (t :: *) where
    RepeatForever :: Microsecond
                  -> (SomeException -> m Microsecond)
                  -> m ()
                  -> RepeatForever m ()

repeatForever :: ( Mockable RepeatForever m )
    => Microsecond
    -> (SomeException -> m Microsecond)
    -> m ()
    -> m ()
repeatForever period handler action =
    liftMockable $ RepeatForever period handler action























data RunInUnboundThread m t where
    RunInUnboundThread :: m t -> RunInUnboundThread m t

runInUnboundThread :: ( Mockable RunInUnboundThread m ) => m t -> m t
runInUnboundThread m = liftMockable $ RunInUnboundThread m





































{-
module Control.TimeWarp.Timed.Misc
       ( repeatForever
       , sleepForever
       ) where

import           Control.Concurrent.STM.TVar       (newTVarIO, readTVarIO, writeTVar)
import           Control.Exception.Base            (SomeException)
import           Control.Monad                     (forever)
import           Control.Monad.Catch               (MonadCatch, catch)
import           Control.Monad.STM                 (atomically)
import           Control.Monad.Trans               (MonadIO, liftIO)

import           Control.TimeWarp.Timed.MonadTimed (Microsecond, MonadTimed, for, fork_,
                                                    minute, ms, startTimer, wait)

-- | Repeats an action periodically.
--   If it fails, handler is invoked, determining delay before retrying.
--   Can be interrupted with asynchronous exception.
repeatForever
    :: (MonadTimed m, MonadIO m, MonadCatch m)
    => Microsecond                      -- ^ Period between action launches
    -> (SomeException -> m Microsecond) -- ^ What to do on exception,
                                        --   returns delay before retrying
    -> m ()                             -- ^ Action
    -> m ()
repeatForever period handler action = do
    timer <- startTimer
    nextDelay <- liftIO $ newTVarIO Nothing
    fork_ $
        let setNextDelay = liftIO . atomically . writeTVar nextDelay . Just
            action' =
                action >> timer >>= \passed -> setNextDelay (period - passed)
            handler' e = handler e >>= setNextDelay
        in action' `catch` handler'
    waitForRes nextDelay
  where
    continue = repeatForever period handler action
    waitForRes nextDelay = do
        wait $ for 10 ms
        res <- liftIO $ readTVarIO nextDelay
        case res of
            Nothing -> waitForRes nextDelay
            Just t  -> wait (for t) >> continue

-- | Sleep forever.

-- TODO: would be better to use `MVar` to block thread
sleepForever :: MonadTimed m => m ()
sleepForever = forever $ wait (for 100500 minute)
-}
