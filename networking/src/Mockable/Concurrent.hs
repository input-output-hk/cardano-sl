{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Mockable.Concurrent (

    ThreadId
  , Fork(..)
  , fork
  , myThreadId
  , throwTo
  , killThread

  , Delay(..)
  , delay
  , sleepForever

  , RunInUnboundThread(..)
  , runInUnboundThread

  , Promise
  , Async(..)
  , async
  , asyncWithUnmask
  , withAsync
  , withAsyncWithUnmask
  , wait
  , cancelWith
  , cancel
  , asyncThreadId
  , race
  , link
  , waitAny
  , waitAnyNonFail
  , waitAnyUnexceptional

  , Concurrently(..)
  , concurrently
  , mapConcurrently
  , forConcurrently

  ) where

import           Control.Exception (AsyncException (..))
import           Control.Exception.Safe (Exception, MonadCatch, catchAny)
import           Data.Time.Units (TimeUnit)
import           Mockable.Class

type family ThreadId (m :: * -> *) :: *

-- | Fork mock to add ability for threads manipulation.
data Fork m t where
    Fork       :: m () -> Fork m (ThreadId m)
    MyThreadId :: Fork m (ThreadId m)
    ThrowTo    :: Exception e => ThreadId m -> e -> Fork m ()

instance (ThreadId n ~ ThreadId m) => MFunctor' Fork m n where
    hoist' nat (Fork action) = Fork $ nat action
    hoist' _ MyThreadId      = MyThreadId
    hoist' _ (ThrowTo tid e) = ThrowTo tid e

----------------------------------------------------------------------------
-- Fork mock helper functions
----------------------------------------------------------------------------

{-# INLINE fork #-}
fork :: ( Mockable Fork m ) => m () -> m (ThreadId m)
fork term = liftMockable $ Fork term

{-# INLINE myThreadId #-}
myThreadId :: ( Mockable Fork m ) => m (ThreadId m)
myThreadId = liftMockable MyThreadId

{-# INLINE throwTo #-}
throwTo :: ( Mockable Fork m, Exception e ) => ThreadId m -> e -> m ()
throwTo tid e = liftMockable $ ThrowTo tid e

{-# INLINE killThread #-}
killThread :: ( Mockable Fork m ) => ThreadId m -> m ()
killThread tid = throwTo tid ThreadKilled

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
    CancelWith :: Exception e => Promise m t -> e -> Async m ()
    AsyncThreadId :: Promise m t -> Async m (ThreadId m)
    Race :: m t -> m r -> Async m (Either t r)
    Link :: Promise m t -> Async m ()
    UnsafeUnmask :: m a -> Async m a

{-# INLINE async #-}
async :: ( Mockable Async m ) => m t -> m (Promise m t)
async m = liftMockable $ Async m

{-# INLINE asyncWithUnmask #-}
asyncWithUnmask :: ( Mockable Async m )
                => ((forall a. m a -> m a) -> m t) -> m (Promise m t)
asyncWithUnmask f = async (f unsafeUnmask)

{-# INLINE withAsync #-}
withAsync :: ( Mockable Async m ) => m t -> (Promise m t -> m r) -> m r
withAsync mterm k = liftMockable $ WithAsync mterm k

{-# INLINE withAsyncWithUnmask #-}
withAsyncWithUnmask
    :: ( Mockable Async m )
    => ((forall a. m a -> m a) -> m t) -> (Promise m t -> m r) -> m r
withAsyncWithUnmask f = withAsync (f unsafeUnmask)

{-# INLINE wait #-}
wait :: ( Mockable Async m ) => Promise m t -> m t
wait promise = liftMockable $ Wait promise

{-# INLINE waitAny #-}
waitAny :: ( Mockable Async m ) => [Promise m t] -> m (Promise m t, t)
waitAny promises = liftMockable $ WaitAny promises

{-# INLINE cancel #-}
cancel :: ( Mockable Async m ) => Promise m t -> m ()
cancel promise = cancelWith promise ThreadKilled

{-# INLINE cancelWith #-}
cancelWith :: ( Mockable Async m, Exception e ) => Promise m t -> e -> m ()
cancelWith promise e = liftMockable $ CancelWith promise e

{-# INLINE asyncThreadId #-}
asyncThreadId :: ( Mockable Async m ) => Promise m t -> m (ThreadId m)
asyncThreadId promise = liftMockable $ AsyncThreadId promise

{-# INLINE race #-}
race :: (Mockable Async m ) => m t -> m r -> m (Either t r)
race a b = liftMockable $ Race a b

{-# INLINE link #-}
link :: ( Mockable Async m ) => Promise m t -> m ()
link promise = liftMockable $ Link promise

{-# INLINE unsafeUnmask #-}
unsafeUnmask :: Mockable Async m => m a -> m a
unsafeUnmask act = liftMockable $ UnsafeUnmask act

instance (Promise n ~ Promise m, ThreadId n ~ ThreadId m) => MFunctor' Async m n where
    hoist' nat (Async act)      = Async $ nat act
    hoist' nat (WithAsync m k)  = WithAsync (nat m) (nat . k)
    hoist' _ (Wait p)           = Wait p
    hoist' _ (WaitAny p)        = WaitAny p
    hoist' _ (CancelWith p e)   = CancelWith p e
    hoist' _ (AsyncThreadId p)  = AsyncThreadId p
    hoist' nat (Race p e)       = Race (nat p) (nat e)
    hoist' _ (Link p)           = Link p
    hoist' nat (UnsafeUnmask m) = UnsafeUnmask (nat m)

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
    :: ( Mockable Async m, MonadCatch m, Eq (Promise m (Maybe a)) )
    => [m a] -> m (Maybe a)
waitAnyUnexceptional acts = impl
  where
    impl = (fmap . fmap) snd $ waitAnyNonFail =<< mapM toAsync acts
    toAsync :: ( Mockable Async m, MonadCatch m ) => m a -> m (Promise m (Maybe a))
    toAsync = async . forPromise
    forPromise :: ( Mockable Async m, MonadCatch m ) => m a -> m (Maybe a)
    forPromise a = (Just <$> a) `catchAny` (const $ pure Nothing)
