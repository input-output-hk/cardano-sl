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
  , sleepForever
  , RepeatForever(..)
  , repeatForever

  , RunInUnboundThread(..)
  , runInUnboundThread

  , Promise
  , Async(..)
  , async
  , wait
  , cancel

  ) where

import Mockable.Class
import Control.TimeWarp.Timed   (RelativeToNow)

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

type family Promise (m :: * -> *) :: * -> *

data Async m t where
    Async :: m t -> Async m (Promise m t)
    Wait :: Promise m t -> Async m t
    Cancel :: Promise m t -> Async m ()

async :: ( Mockable Async m ) => m t -> m (Promise m t)
async m = liftMockable $ Async m

wait :: ( Mockable Async m ) => Promise m t -> m t
wait promise = liftMockable $ Wait promise

cancel :: ( Mockable Async m ) => Promise m t -> m ()
cancel promise = liftMockable $ Cancel promise
