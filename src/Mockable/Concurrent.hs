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
  , for
  , hour
  , minute
  , sec
  , ms
  , mcs
  , sleepForever
  , RepeatForever(..)
  , repeatForever

  , RunInUnboundThread(..)
  , runInUnboundThread

  ) where

import Mockable.Class
import Control.TimeWarp.Timed   (RelativeToNow, for, hour, minute, sec, ms, mcs)

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

data Delay (m :: * -> *) (t :: *) where
    Delay :: RelativeToNow -> Delay m ()    -- Finite delay.
    SleepForever :: Delay m ()              -- Infinite delay.

wait :: ( Mockable Delay m ) => RelativeToNow -> m ()
wait relativeToNow = liftMockable $ Delay relativeToNow

sleepForever :: ( Mockable Delay m ) => m ()
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
