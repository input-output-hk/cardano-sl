{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Mockable.Concurrent (

    ThreadId
  , Fork(..)
  , fork
  , myThreadId
  , killThread

  , Delay(..)
  , RelativeToNow
  , delay
  , for
  , sleepForever

  , CurrentTime(..)
  , currentTime

  , RunInUnboundThread(..)
  , runInUnboundThread

  , Promise
  , Async(..)
  , async
  , wait
  , cancel

  , Concurrently(..)
  , concurrently
  , mapConcurrently
  , forConcurrently

  ) where

import           Control.Exception.Base (SomeException)
import           Control.Monad.Reader   (ReaderT)
import           Data.Time.Units        (Microsecond)
import           Mockable.Class

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

type RelativeToNow = Microsecond -> Microsecond

data Delay (m :: * -> *) (t :: *) where
    Delay :: RelativeToNow -> Delay m ()    -- Finite delay.
    SleepForever :: Delay m ()              -- Infinite delay.

instance MFunctor' Delay m n where
    hoist' _ (Delay i)    = Delay i
    hoist' _ SleepForever = SleepForever

delay :: ( Mockable Delay m ) => RelativeToNow -> m ()
delay relativeToNow = liftMockable $ Delay relativeToNow

for :: Microsecond -> RelativeToNow
for = (+)

sleepForever :: ( Mockable Delay m ) => m ()
sleepForever = liftMockable SleepForever

data CurrentTime (m :: * -> *) (t :: *) where
    CurrentTime :: CurrentTime m Microsecond

instance MFunctor' CurrentTime m n where
    hoist' _ CurrentTime = CurrentTime

currentTime :: ( Mockable CurrentTime m ) => m Microsecond
currentTime = liftMockable CurrentTime

data RunInUnboundThread m t where
    RunInUnboundThread :: m t -> RunInUnboundThread m t

instance MFunctor' RunInUnboundThread m n where
    hoist' nat (RunInUnboundThread action) = RunInUnboundThread $ nat action

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

instance (Promise n ~ Promise m) => MFunctor' Async m n where
    hoist' nat (Async act) = Async $ nat act
    hoist' _ (Wait p)      = Wait p
    hoist' _ (Cancel p)    = Cancel p

data Concurrently m t where
    Concurrently :: m a -> m b -> Concurrently m (a, b)

instance MFunctor' Concurrently m n where
    hoist' nat (Concurrently a b) = Concurrently (nat a) (nat b)

concurrently :: ( Mockable Concurrently m ) => m a -> m b -> m (a, b)
concurrently a b = liftMockable $ Concurrently a b

newtype ConcurrentlyA m t = ConcurrentlyA {
      runConcurrentlyA :: m t
    }

instance ( Functor m ) => Functor (ConcurrentlyA m) where
    fmap f = ConcurrentlyA . fmap f . runConcurrentlyA

instance ( Mockable Concurrently m ) => Applicative (ConcurrentlyA m) where
    pure = ConcurrentlyA . pure
    cf <*> cx = ConcurrentlyA $ do
        (f, x) <- concurrently (runConcurrentlyA cf) (runConcurrentlyA cx)
        pure $ f x

mapConcurrently
    :: ( Traversable f, Mockable Concurrently m )
    => (s -> m t)
    -> f s
    -> m (f t)
mapConcurrently g = runConcurrentlyA . traverse (ConcurrentlyA . g)

forConcurrently
    :: ( Traversable f, Mockable Concurrently m )
    => f s
    -> (s -> m t)
    -> m (f t)
forConcurrently = flip mapConcurrently
