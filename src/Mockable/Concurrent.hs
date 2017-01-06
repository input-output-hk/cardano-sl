{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

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

import           Control.Exception.Base (SomeException)
import           Control.Monad.Reader   (ReaderT)
import           Control.TimeWarp.Timed (RelativeToNow, for, hour, mcs, minute, ms, sec)
import           Data.Time.Units        (Microsecond)

import           Mockable.Class         (Mockable (..))


type family ThreadId (m :: * -> *) :: *

-- | Fork mock to add ability for threads manipulation.
data Fork m t where
    Fork       :: m () -> Fork m (ThreadId m)
    MyThreadId :: Fork m (ThreadId m)
    KillThread :: ThreadId m -> Fork m ()

----------------------------------------------------------------------------
-- Fork mock helper functions
----------------------------------------------------------------------------

fork :: ( Mockable Fork m ) => m () -> m (ThreadId m)
fork term = liftMockable $ Fork term

myThreadId :: ( Mockable Fork m ) => m (ThreadId m)
myThreadId = liftMockable MyThreadId

killThread :: ( Mockable Fork m ) => ThreadId m -> m ()
killThread tid = liftMockable $ KillThread tid

----------------------------------------------------------------------------
-- Standard Fork instances
----------------------------------------------------------------------------

instance Mockable Fork m => Mockable Fork (ReaderT r m) where
    liftMockable (Fork m)         = fork m
    liftMockable MyThreadId       = myThreadId
    liftMockable (KillThread tid) = killThread tid

-- | Delay mock to add ability to delay execution.
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
