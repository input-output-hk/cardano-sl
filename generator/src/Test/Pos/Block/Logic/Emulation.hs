{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}

-- | Base monad used in tests, some real-world functionality is emulated.
-- It's called 'Emulation' to follow the style used in node-sketch, where
-- real-world analogue is called 'Production'.

module Test.Pos.Block.Logic.Emulation
       ( ClockVar (..)
       , Emulation (..)
       , runEmulation
       , sudoLiftIO
       ) where

import           Universum

import           Control.Monad.Base (MonadBase (..))
import qualified Control.Monad.Trans.Control as MC
import qualified Crypto.Random as Rand
import           Data.Coerce (coerce)
import           Data.Time.Units (Microsecond)
import           Mockable (Async, Channel, ChannelT, Concurrently, CurrentTime (..), Delay (..),
                           Fork, MFunctor' (hoist'), Mockable (..), Production (..), Promise,
                           SharedAtomic (..), SharedAtomicT, SharedExclusive (..), SharedExclusiveT,
                           ThreadId)
import qualified Mockable.Metrics as Metrics
import           System.Wlog (CanLog (..))

newtype ClockVar = ClockVar (IORef Microsecond)

newtype Emulation a = Emulation { unEmulation :: ReaderT ClockVar IO a }
  deriving
    (Functor, Applicative, Monad, MonadThrow, MonadCatch, MonadMask)

instance Rand.MonadRandom Emulation where
    getRandomBytes = Emulation . lift . Rand.getRandomBytes

runEmulation :: Microsecond -> Emulation a -> IO a
runEmulation startTime m = do
    clockVar <- newIORef startTime
    runReaderT (unEmulation m) (ClockVar clockVar)

-- Lift IO without a warning.
sudoLiftIO :: IO a -> Emulation a
sudoLiftIO m = Emulation (liftIO m)

instance MonadIO Emulation where
    liftIO m = Emulation . liftIO $ do
        -- if you see a lot of stars in the test log, a hunting season for
        -- not-mocked operations is open.
        --
        -- It's currently commented because stars take too much buffer space
        -- (all of my tmux @volhovm)
        --
        -- putStr ("*" :: String)
        m

instance MonadBase IO Emulation where
    liftBase = liftIO

type LiftBaseWith b m a = (MC.RunInBase m b -> b a) -> m a

newtype LiftBaseWith' b m a = LBW { unLBW :: LiftBaseWith b m a }

coerceLiftBaseWith ::
    LiftBaseWith b (ReaderT ClockVar IO) a ->
    LiftBaseWith b Emulation             a
coerceLiftBaseWith lbw =
    unLBW (coerce (LBW lbw))

-- Bad instance! Bad! Kill it!
-- NB. the instance is correct, but
--    we don't need no IO actions
--    we don't need no flow control
--    no damn exceptions in the test logs
--    hey, -------, leave the code alone.
instance MC.MonadBaseControl IO Emulation where
    type StM Emulation a = a
    liftBaseWith = coerceLiftBaseWith MC.liftBaseWith
    restoreM =
      (coerce :: forall a .
        (a -> ReaderT ClockVar IO a) ->
        (a -> Emulation a))
      MC.restoreM

instance CanLog Emulation where
    dispatchMessage ln s t = Emulation $ dispatchMessage ln s t

----------------------------------------------------------------------------
-- Time emulation
----------------------------------------------------------------------------

-- [CSL-1376] FIXME: make it work!

-- instance Mockable CurrentTime Emulation where
--     liftMockable CurrentTime = Emulation $ do
--         ClockVar clockVar <- ask
--         readIORef clockVar

-- -- The tests compile even without this instance, meaning we don't even test
-- -- delays, which is sad.
-- instance Mockable Delay Emulation where
--     liftMockable SleepForever = return ()
--     liftMockable (Delay d) = Emulation $ do
--         ClockVar clockVar <- ask
--         atomicModifyIORef' clockVar (\t -> (addTime t d, ()))

instance Mockable CurrentTime Emulation where
    {-# INLINABLE liftMockable #-}
    {-# SPECIALIZE INLINE liftMockable :: CurrentTime Emulation t -> Emulation t #-}
    liftMockable = liftMockableProduction

instance Mockable Delay Emulation where
    {-# INLINABLE liftMockable #-}
    {-# SPECIALIZE INLINE liftMockable :: Delay Emulation t -> Emulation t #-}
    liftMockable = liftMockableProduction

----------------------------------------------------------------------------
-- Helper to use 'Production' implementation
----------------------------------------------------------------------------

liftMockableProduction ::
       (Mockable d Production, MFunctor' d Emulation Production)
    => d Emulation t
    -> Emulation t
liftMockableProduction dmt =
    Emulation . ReaderT $ \r ->
        runProduction $ liftMockable $ hoist' (hoistF r) dmt
  where
    hoistF :: ClockVar -> Emulation a -> Production a
    hoistF ctx = Production . usingReaderT ctx . unEmulation

----------------------------------------------------------------------------
-- Metrics, SharedExclusive and other stuff // life is hard
----------------------------------------------------------------------------
-- For this stuff we use Production implementation because we are not smart
-- enough to implement something else.
----------------------------------------------------------------------------

type instance Metrics.Counter Emulation = Metrics.Counter Production
type instance Metrics.Gauge Emulation = Metrics.Gauge Production
type instance Metrics.Distribution Emulation = Metrics.Distribution Production

instance Mockable Metrics.Metrics Emulation where
    {-# INLINABLE liftMockable #-}
    {-# SPECIALIZE INLINE liftMockable :: Metrics.Metrics Emulation t -> Emulation t #-}
    liftMockable = liftMockableProduction

type instance SharedExclusiveT Emulation = SharedExclusiveT Production

instance Mockable SharedExclusive Emulation where
    {-# INLINABLE liftMockable #-}
    {-# SPECIALIZE INLINE liftMockable :: SharedExclusive Emulation t -> Emulation t #-}
    liftMockable = liftMockableProduction

type instance SharedAtomicT Emulation = SharedAtomicT Production

instance Mockable SharedAtomic Emulation where
    {-# INLINABLE liftMockable #-}
    {-# SPECIALIZE INLINE liftMockable :: SharedAtomic Emulation t -> Emulation t #-}
    liftMockable = liftMockableProduction

type instance ThreadId Emulation = ThreadId Production

instance Mockable Fork Emulation where
    {-# INLINABLE liftMockable #-}
    -- {-# SPECIALIZE INLINE liftMockable :: Fork Emulation t -> Fork t #-}
    liftMockable = liftMockableProduction

type instance Promise Emulation = Promise Production

instance Mockable Async Emulation where
    {-# INLINABLE liftMockable #-}
    {-# SPECIALIZE INLINE liftMockable :: Async Emulation t -> Emulation t #-}
    liftMockable = liftMockableProduction

instance Mockable Concurrently Emulation where
    {-# INLINABLE liftMockable #-}
    {-# SPECIALIZE INLINE liftMockable :: Concurrently Emulation t -> Emulation t #-}
    liftMockable = liftMockableProduction

type instance ChannelT Emulation = ChannelT Production

instance Mockable Channel Emulation where
    {-# INLINABLE liftMockable #-}
    {-# SPECIALIZE INLINE liftMockable :: Channel Emulation t -> Emulation t #-}
    liftMockable = liftMockableProduction
