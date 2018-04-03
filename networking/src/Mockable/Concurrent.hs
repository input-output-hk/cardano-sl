{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Mockable.Concurrent (

    ThreadId
  , Fork(..)
  , fork
  , throwTo
  , killThread

  , Delay(..)
  , delay

  , MyThreadId(..)
  , myThreadId

  , RunInUnboundThread(..)
  , runInUnboundThread

  , Promise
  , Async(..)
  , withAsync
  , withAsyncWithUnmask
  , asyncThreadId
  , race

  , Concurrently(..)
  , concurrently
  , mapConcurrently
  , forConcurrently

  , LowLevelAsync(..)
  , async
  , asyncWithUnmask
  , cancelWith
  , cancel
  , link
  , wait
  , waitAny

  -- * Utility functions
  , timeout
  ) where

import           Universum

import           Control.Exception (Exception, AsyncException (..))
import           Data.Time.Units (TimeUnit)
import           Mockable.Class

----------------------------------------------------------------------------
-- Type families
----------------------------------------------------------------------------

type family ThreadId (m :: * -> *) :: *
type family Promise (m :: * -> *) :: * -> *

----------------------------------------------------------------------------
-- Fork mock
----------------------------------------------------------------------------

-- | Fork mock to add ability for low-level threads
-- manipulation. Usually you should prefer 'Async' mock for threads
-- manipulation.
data Fork m t where
    Fork       :: m () -> Fork m (ThreadId m)
    ThrowTo    :: Exception e => ThreadId m -> e -> Fork m ()

instance (ThreadId n ~ ThreadId m) => MFunctor' Fork m n where
    hoist' nat (Fork action) = Fork $ nat action
    hoist' _ (ThrowTo tid e) = ThrowTo tid e

----------------------------------------------------------------------------
-- Fork mock helper functions
----------------------------------------------------------------------------

{-# INLINE fork #-}
fork :: ( Mockable Fork m ) => m () -> m (ThreadId m)
fork term = liftMockable $ Fork term

{-# INLINE throwTo #-}
throwTo :: ( Mockable Fork m, Exception e ) => ThreadId m -> e -> m ()
throwTo tid e = liftMockable $ ThrowTo tid e

{-# INLINE killThread #-}
killThread :: ( Mockable Fork m ) => ThreadId m -> m ()
killThread tid = throwTo tid ThreadKilled

----------------------------------------------------------------------------
-- Delay mock
----------------------------------------------------------------------------

data Delay (m :: * -> *) (t :: *) where
    Delay :: TimeUnit t => t -> Delay m ()    -- Finite delay.

instance MFunctor' Delay m n where
    hoist' _ (Delay i)    = Delay i

----------------------------------------------------------------------------
-- Delay mock helper functions
----------------------------------------------------------------------------

{-# INLINE delay #-}
delay :: ( Mockable Delay m ) => TimeUnit t => t -> m ()
delay time = liftMockable $ Delay time

----------------------------------------------------------------------------
-- MyThreadId mock and helper functions
----------------------------------------------------------------------------

data MyThreadId m t where
    MyThreadId :: MyThreadId m (ThreadId m)

instance (ThreadId n ~ ThreadId m) => MFunctor' MyThreadId m n where
    hoist' _ MyThreadId      = MyThreadId

{-# INLINE myThreadId #-}
myThreadId :: ( Mockable MyThreadId m ) => m (ThreadId m)
myThreadId = liftMockable MyThreadId

----------------------------------------------------------------------------
-- RunInUnboundThread mock and helper functions
----------------------------------------------------------------------------

data RunInUnboundThread m t where
    RunInUnboundThread :: m t -> RunInUnboundThread m t

instance MFunctor' RunInUnboundThread m n where
    hoist' nat (RunInUnboundThread action) = RunInUnboundThread $ nat action

{-# INLINE runInUnboundThread #-}
runInUnboundThread :: ( Mockable RunInUnboundThread m ) => m t -> m t
runInUnboundThread m = liftMockable $ RunInUnboundThread m

----------------------------------------------------------------------------
-- High-level Async mock and helper functions
----------------------------------------------------------------------------

-- | 'Async' mock which mimics functions from `async` library. It
-- intentionally does not contain 'async' function, because it
-- generally should not be used (there is e. g. 'withAsync' function
-- which is safer).
data Async m t where
    WithAsync :: m t -> (Promise m t -> m r) -> Async m r
    AsyncThreadId :: Promise m t -> Async m (ThreadId m)
    Race :: m t -> m r -> Async m (Either t r)
    -- | This is needed to implement 'withAsyncWithUnmask'. Note that
    -- 'unsafeUnmask' function is not exported.
    UnsafeUnmask :: m a -> Async m a

{-# INLINE withAsync #-}
withAsync :: ( Mockable Async m ) => m t -> (Promise m t -> m r) -> m r
withAsync mterm k = liftMockable $ WithAsync mterm k

{-# INLINE withAsyncWithUnmask #-}
withAsyncWithUnmask
    :: ( Mockable Async m )
    => ((forall a. m a -> m a) -> m t) -> (Promise m t -> m r) -> m r
withAsyncWithUnmask f = withAsync (f unsafeUnmask)

{-# INLINE asyncThreadId #-}
asyncThreadId :: ( Mockable Async m ) => Promise m t -> m (ThreadId m)
asyncThreadId promise = liftMockable $ AsyncThreadId promise

{-# INLINE race #-}
race :: (Mockable Async m ) => m t -> m r -> m (Either t r)
race a b = liftMockable $ Race a b

{-# INLINE unsafeUnmask #-}
unsafeUnmask :: Mockable Async m => m a -> m a
unsafeUnmask act = liftMockable $ UnsafeUnmask act

instance (Promise n ~ Promise m, ThreadId n ~ ThreadId m) => MFunctor' Async m n where
    hoist' nat (WithAsync m k)  = WithAsync (nat m) (nat . k)
    hoist' _ (AsyncThreadId p)  = AsyncThreadId p
    hoist' nat (Race p e)       = Race (nat p) (nat e)
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

----------------------------------------------------------------------------
-- Low-level Async mock and helper functions
----------------------------------------------------------------------------

data LowLevelAsync m t where
    Async :: m t -> LowLevelAsync m (Promise m t)
    Link :: Promise m t -> LowLevelAsync m ()
    Wait :: Promise m t -> LowLevelAsync m t
    WaitAny :: [Promise m t] -> LowLevelAsync m (Promise m t, t)
    CancelWith :: Exception e => Promise m t -> e -> LowLevelAsync m ()

{-# INLINE async #-}
async :: ( Mockable LowLevelAsync m ) => m t -> m (Promise m t)
async m = liftMockable $ Async m

{-# INLINE asyncWithUnmask #-}
asyncWithUnmask :: ( Mockable Async m, Mockable LowLevelAsync m )
                => ((forall a. m a -> m a) -> m t) -> m (Promise m t)
asyncWithUnmask f = async (f unsafeUnmask)

{-# INLINE link #-}
link :: ( Mockable LowLevelAsync m ) => Promise m t -> m ()
link promise = liftMockable $ Link promise

{-# INLINE wait #-}
wait :: ( Mockable LowLevelAsync m ) => Promise m t -> m t
wait promise = liftMockable $ Wait promise

{-# INLINE waitAny #-}
waitAny :: ( Mockable LowLevelAsync m ) => [Promise m t] -> m (Promise m t, t)
waitAny promises = liftMockable $ WaitAny promises

{-# INLINE cancel #-}
cancel :: ( Mockable LowLevelAsync m ) => Promise m t -> m ()
cancel promise = cancelWith promise ThreadKilled

{-# INLINE cancelWith #-}
cancelWith :: ( Mockable LowLevelAsync m, Exception e ) => Promise m t -> e -> m ()
cancelWith promise e = liftMockable $ CancelWith promise e

instance (Promise n ~ Promise m, ThreadId n ~ ThreadId m) => MFunctor' LowLevelAsync m n where
    hoist' nat (Async act)    = Async $ nat act
    hoist' _ (Link p)         = Link p
    hoist' _ (Wait p)         = Wait p
    hoist' _ (WaitAny p)      = WaitAny p
    hoist' _ (CancelWith p e) = CancelWith p e

----------------------------------------------------------------------------
-- Other utility functions
----------------------------------------------------------------------------

-- | This function is analogous to `System.Timeout.timeout`, it's
-- based on `Race` and `Delay`.
{-# INLINE timeout #-}
timeout :: (Mockable Delay m, Mockable Async m, TimeUnit t) => t -> m a -> m (Maybe a)
timeout t ma = rightToMaybe <$> race (delay t) ma
