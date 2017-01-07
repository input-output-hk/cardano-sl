{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
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
import           Control.Monad.Reader   (ReaderT (..))
import           Control.TimeWarp.Timed (RelativeToNow, for, hour, mcs, minute, ms, sec)
import           Data.Time.Units        (Microsecond)

import           Mockable.Class         (MFunctor' (hoist'), Mockable (liftMockable))

type family ThreadId (m :: * -> *) :: *
type instance ThreadId (ReaderT r m) = ThreadId m

-- | Fork mock to add ability for threads manipulation.
data Fork m t where
    Fork       :: m () -> Fork m (ThreadId m)
    MyThreadId :: Fork m (ThreadId m)
    KillThread :: ThreadId m -> Fork m ()

instance (ThreadId n ~ ThreadId m) => MFunctor' Fork m n where
    hoist' nat (Fork action)  = Fork $ nat action
    hoist' _ MyThreadId       = MyThreadId
    hoist' _ (KillThread tid) = KillThread tid

----------------------------------------------------------------------------
-- Fork mock helper functions
----------------------------------------------------------------------------

fork :: ( Mockable Fork m ) => m () -> m (ThreadId m)
fork term = liftMockable $ Fork term

myThreadId :: ( Mockable Fork m ) => m (ThreadId m)
myThreadId = liftMockable MyThreadId

killThread :: ( Mockable Fork m ) => ThreadId m -> m ()
killThread tid = liftMockable $ KillThread tid

-- | Delay mock to add ability to delay execution.
data Delay (m :: * -> *) (t :: *) where
    Delay :: RelativeToNow -> Delay m ()    -- Finite delay.
    SleepForever :: Delay m ()              -- Infinite delay.

instance MFunctor' Delay m n where
    hoist' _ (Delay i)    = Delay i
    hoist' _ SleepForever = SleepForever

wait :: ( Mockable Delay m ) => RelativeToNow -> m ()
wait relativeToNow = liftMockable $ Delay relativeToNow

sleepForever :: ( Mockable Delay m ) => m ()
sleepForever = liftMockable SleepForever

data RepeatForever (m :: * -> *) (t :: *) where
    RepeatForever :: Microsecond
                  -> (SomeException -> m Microsecond)
                  -> m ()
                  -> RepeatForever m ()

instance MFunctor' RepeatForever m n where
    hoist' nat (RepeatForever time eh action) =
        RepeatForever time (\ex -> nat $ eh ex) (nat action)

repeatForever :: ( Mockable RepeatForever m )
    => Microsecond
    -> (SomeException -> m Microsecond)
    -> m ()
    -> m ()
repeatForever period handler action =
    liftMockable $ RepeatForever period handler action

data RunInUnboundThread m t where
    RunInUnboundThread :: m t -> RunInUnboundThread m t

instance MFunctor' RunInUnboundThread m n where
    hoist' nat (RunInUnboundThread action) = RunInUnboundThread $ nat action

runInUnboundThread :: ( Mockable RunInUnboundThread m ) => m t -> m t
runInUnboundThread m = liftMockable $ RunInUnboundThread m
