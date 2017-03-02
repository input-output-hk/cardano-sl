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
  , delay
  , sleepForever

  , RunInUnboundThread(..)
  , runInUnboundThread

  , Promise
  , Async(..)
  , async
  , withAsync
  , wait
  , cancel
  , asyncThreadId
  , waitAny
  , waitAnyNonFail
  , waitAnyUnexceptional

  , Concurrently(..)
  , concurrently
  , mapConcurrently
  , forConcurrently

  ) where

import           Data.Time.Units    (TimeUnit)
import           Mockable.Class
import           Mockable.Exception (Catch, catchAll)

type family ThreadId (m :: * -> *) :: *

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

{-# INLINE fork #-}
fork :: ( Mockable Fork m ) => m () -> m (ThreadId m)
fork term = liftMockable $ Fork term

{-# INLINE myThreadId #-}
myThreadId :: ( Mockable Fork m ) => m (ThreadId m)
myThreadId = liftMockable MyThreadId

{-# INLINE killThread #-}
killThread :: ( Mockable Fork m ) => ThreadId m -> m ()
killThread tid = liftMockable $ KillThread tid

data Delay (m :: * -> *) (t :: *) where
    Delay :: TimeUnit t => t -> Delay m ()    -- Finite delay.
    SleepForever :: Delay m ()                -- Infinite delay.

instance MFunctor' Delay m n where
    hoist' _ (Delay i)    = Delay i
    hoist' _ SleepForever = SleepForever

{-# INLINE delay #-}
delay :: ( Mockable Delay m ) => TimeUnit t => t -> m ()
delay time = liftMockable $ Delay time

{-# INLINE sleepForever #-}
sleepForever :: ( Mockable Delay m ) => m ()
sleepForever = liftMockable SleepForever

data RunInUnboundThread m t where
    RunInUnboundThread :: m t -> RunInUnboundThread m t

instance MFunctor' RunInUnboundThread m n where
    hoist' nat (RunInUnboundThread action) = RunInUnboundThread $ nat action

{-# INLINE runInUnboundThread #-}
runInUnboundThread :: ( Mockable RunInUnboundThread m ) => m t -> m t
runInUnboundThread m = liftMockable $ RunInUnboundThread m

type family Promise (m :: * -> *) :: * -> *

data Async m t where
    Async :: m t -> Async m (Promise m t)
    WithAsync :: m t -> (Promise m t -> m r) -> Async m r
    Wait :: Promise m t -> Async m t
    WaitAny :: [Promise m t] -> Async m (Promise m t, t)
    Cancel :: Promise m t -> Async m ()
    AsyncThreadId :: Promise m t -> Async m (ThreadId m)

{-# INLINE async #-}
async :: ( Mockable Async m ) => m t -> m (Promise m t)
async m = liftMockable $ Async m

{-# INLINE withAsync #-}
withAsync :: ( Mockable Async m ) => m t -> (Promise m t -> m r) -> m r
withAsync mterm k = liftMockable $ WithAsync mterm k

{-# INLINE wait #-}
wait :: ( Mockable Async m ) => Promise m t -> m t
wait promise = liftMockable $ Wait promise

{-# INLINE waitAny #-}
waitAny :: ( Mockable Async m ) => [Promise m t] -> m (Promise m t, t)
waitAny promises = liftMockable $ WaitAny promises

{-# INLINE cancel #-}
cancel :: ( Mockable Async m ) => Promise m t -> m ()
cancel promise = liftMockable $ Cancel promise

{-# INLINE asyncThreadId #-}
asyncThreadId :: ( Mockable Async m ) => Promise m t -> m (ThreadId m)
asyncThreadId promise = liftMockable $ AsyncThreadId promise

instance (Promise n ~ Promise m, ThreadId n ~ ThreadId m) => MFunctor' Async m n where
    hoist' nat (Async act)     = Async $ nat act
    hoist' nat (WithAsync m k) = WithAsync (nat m) (nat . k)
    hoist' _ (Wait p)          = Wait p
    hoist' _ (WaitAny p)       = WaitAny p
    hoist' _ (Cancel p)        = Cancel p
    hoist' _ (AsyncThreadId p) = AsyncThreadId p

data Concurrently m t where
    Concurrently :: m a -> m b -> Concurrently m (a, b)

instance MFunctor' Concurrently m n where
    hoist' nat (Concurrently a b) = Concurrently (nat a) (nat b)

{-# INLINE concurrently #-}
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

{-# INLINE mapConcurrently #-}
mapConcurrently
    :: ( Traversable f, Mockable Concurrently m )
    => (s -> m t)
    -> f s
    -> m (f t)
mapConcurrently g = runConcurrentlyA . traverse (ConcurrentlyA . g)

{-# INLINE forConcurrently #-}
forConcurrently
    :: ( Traversable f, Mockable Concurrently m )
    => f s
    -> (s -> m t)
    -> m (f t)
forConcurrently = flip mapConcurrently

{-# INLINE waitAnyNonFail #-}
waitAnyNonFail
    :: ( Mockable Async m, Eq (Promise m (Maybe a)) )
    => [ Promise m (Maybe a) ] -> m (Maybe (Promise m (Maybe a), a))
waitAnyNonFail promises = waitAny promises >>= handleRes
  where
    handleRes (p, Just res) = pure $ Just (p, res)
    handleRes (p, _)        = waitAnyNonFail (filter (/= p) promises)

{-# INLINE waitAnyUnexceptional #-}
waitAnyUnexceptional
    :: ( Mockable Async m, Mockable Catch m, Eq (Promise m (Maybe a)) )
    => [m a] -> m (Maybe a)
waitAnyUnexceptional acts = impl
  where
    impl = (fmap . fmap) snd $ waitAnyNonFail =<< mapM toAsync acts
    toAsync :: ( Mockable Async m, Mockable Catch m ) => m a -> m (Promise m (Maybe a))
    toAsync = async . forPromise
    forPromise :: ( Mockable Async m, Mockable Catch m ) => m a -> m (Maybe a)
    forPromise a = (Just <$> a) `catchAll` (const $ pure Nothing)
