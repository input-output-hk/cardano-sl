{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}

module Mockable.Production
       ( Production (..)
       ) where

import qualified Control.Concurrent as Conc
import qualified Control.Concurrent.Async as Conc
import qualified Control.Concurrent.STM as Conc
import           Control.Exception.Safe (MonadCatch, MonadMask (..), MonadThrow)
import qualified Control.Exception.Safe as Exception
import           Control.Monad.Fix (MonadFix)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.IO.Unlift (MonadUnliftIO (..))
import qualified Crypto.Random as Rand
import           Data.Time.Units (toMicroseconds)
import qualified GHC.IO as GHC
import qualified System.Metrics.Counter as EKG.Counter
import qualified System.Metrics.Distribution as EKG.Distribution
import qualified System.Metrics.Gauge as EKG.Gauge
import           System.Wlog (CanLog (..), HasLoggerName (..))

import           Control.Monad.Base (MonadBase (..))
import           Control.Monad.Trans.Control (MonadBaseControl (..))
import           Mockable.Channel (Channel (..), ChannelT)
import           Mockable.Class (Mockable (..))
import           Mockable.Concurrent (Async (..), Concurrently (..), Delay (..), Fork (..),
                                      LowLevelAsync (..), MyThreadId (..), Promise,
                                      RunInUnboundThread (..), ThreadId)
import           Mockable.CurrentTime (CurrentTime (..), realTime)
import qualified Mockable.Metrics as Metrics
import           Mockable.SharedAtomic (SharedAtomic (..), SharedAtomicT)
import           Mockable.SharedExclusive (SharedExclusive (..), SharedExclusiveT)

newtype Production t = Production
    { runProduction :: IO t
    } deriving (Functor, Applicative, Monad)

deriving instance MonadIO Production
deriving instance MonadUnliftIO Production
deriving instance MonadFix Production
deriving instance MonadThrow Production
deriving instance MonadCatch Production
deriving instance CanLog Production
deriving instance Rand.MonadRandom Production

type instance ThreadId Production = Conc.ThreadId

instance Mockable Fork Production where
    {-# INLINABLE liftMockable #-}
    {-# SPECIALIZE INLINE liftMockable :: Fork Production t -> Production t #-}
    liftMockable (Fork m)        = Production $ Conc.forkIO (runProduction m)
    liftMockable (ThrowTo tid e) = Production $ Conc.throwTo tid e

instance Mockable Delay Production where
    {-# INLINABLE liftMockable #-}
    {-# SPECIALIZE INLINE liftMockable :: Delay Production t -> Production t #-}
    liftMockable (Delay time) = Production $
        -- toMicroseconds :: TimeUnit t => t -> Integer
        -- then we cast to an Int. Hopefully it fits!
        Conc.threadDelay (fromIntegral (toMicroseconds time))

instance Mockable MyThreadId Production where
    {-# INLINABLE liftMockable #-}
    {-# SPECIALIZE INLINE liftMockable :: MyThreadId Production t -> Production t #-}
    liftMockable (MyThreadId)    = Production $ Conc.myThreadId

instance Mockable RunInUnboundThread Production where
    {-# INLINABLE liftMockable #-}
    {-# SPECIALIZE INLINE liftMockable :: RunInUnboundThread Production t -> Production t #-}
    liftMockable (RunInUnboundThread m) = Production $
        Conc.runInUnboundThread (runProduction m)

instance Mockable CurrentTime Production where
    {-# INLINABLE liftMockable #-}
    {-# SPECIALIZE INLINE liftMockable :: CurrentTime Production t -> Production t #-}
    liftMockable CurrentTime = realTime

type instance Promise Production = Conc.Async

instance Mockable Async Production where
    {-# INLINABLE liftMockable #-}
    {-# SPECIALIZE INLINE liftMockable :: Async Production t -> Production t #-}
    liftMockable (WithAsync m k)        = Production $ Conc.withAsync (runProduction m) (runProduction . k)
    liftMockable (AsyncThreadId p)      = Production $ return (Conc.asyncThreadId p)
    liftMockable (Race a b)             = Production $ Conc.race (runProduction a) (runProduction b)
    liftMockable (UnsafeUnmask act)     = Production $
        GHC.unsafeUnmask (runProduction act)

instance Mockable LowLevelAsync Production where
    {-# INLINABLE liftMockable #-}
    {-# SPECIALIZE INLINE liftMockable :: LowLevelAsync Production t -> Production t #-}
    liftMockable (Async m)              = Production $ Conc.async (runProduction m)
    liftMockable (Link p)               = Production $ Conc.link p
    liftMockable (Wait promise)         = Production $ Conc.wait promise
    liftMockable (WaitAny promises)     = Production $ Conc.waitAny promises
    liftMockable (CancelWith promise e) = Production $ Conc.cancelWith promise e

instance Mockable Concurrently Production where
    {-# INLINABLE liftMockable #-}
    {-# SPECIALIZE INLINE liftMockable :: Concurrently Production t -> Production t #-}
    liftMockable (Concurrently a b) = Production $
        Conc.concurrently (runProduction a) (runProduction b)

type instance SharedAtomicT Production = Conc.MVar

instance Mockable SharedAtomic Production where
    {-# INLINABLE liftMockable #-}
    {-# SPECIALIZE INLINE liftMockable :: SharedAtomic Production t -> Production t #-}
    liftMockable (NewSharedAtomic t)
        = Production $ Conc.newMVar t
    liftMockable (ModifySharedAtomic atomic f)
        = Production $ Conc.modifyMVar atomic (runProduction . f)
    liftMockable (ReadSharedAtomic atomic)
        = Production $ Conc.readMVar atomic

type instance SharedExclusiveT Production = Conc.MVar

instance Mockable SharedExclusive Production where
    {-# INLINABLE liftMockable #-}
    {-# SPECIALIZE INLINE liftMockable :: SharedExclusive Production t -> Production t #-}
    liftMockable (NewSharedExclusive)
        = Production $ Conc.newEmptyMVar
    liftMockable (PutSharedExclusive var t)
        = Production $ Conc.putMVar var t
    liftMockable (TakeSharedExclusive var)
        = Production $ Conc.takeMVar var
    liftMockable (ModifySharedExclusive var f)
        = Production $ Conc.modifyMVar var (runProduction . f)
    liftMockable (TryPutSharedExclusive var t)
        = Production $ Conc.tryPutMVar var t

type instance ChannelT Production = Conc.TChan

instance Mockable Channel Production where
    {-# INLINABLE liftMockable #-}
    {-# SPECIALIZE INLINE liftMockable :: Channel Production t -> Production t #-}
    liftMockable (NewChannel) = Production . Conc.atomically $ Conc.newTChan
    liftMockable (ReadChannel channel) = Production . Conc.atomically $ Conc.readTChan channel
    liftMockable (TryReadChannel channel) = Production . Conc.atomically $ Conc.tryReadTChan channel
    liftMockable (UnGetChannel channel t) = Production . Conc.atomically $ Conc.unGetTChan channel t
    liftMockable (WriteChannel channel t) = Production . Conc.atomically $ Conc.writeTChan channel t

newtype FailException = FailException String

deriving instance Show FailException
instance Exception.Exception FailException

instance MonadMask Production where
    mask act = Production $ mask $
        \unmask -> runProduction $ act $ Production . unmask . runProduction
    uninterruptibleMask act = Production $ uninterruptibleMask $
        \unmask -> runProduction $ act $ Production . unmask . runProduction

instance HasLoggerName Production where
    askLoggerName = return "*production*"
    modifyLoggerName = const id

instance MonadBase IO Production where
    liftBase = Production

instance MonadBaseControl IO Production where
    type StM Production a = a
    liftBaseWith f = Production $ liftBaseWith $ \q -> f (q . runProduction)
    restoreM = Production . pure

type instance Metrics.Counter Production = EKG.Counter.Counter
type instance Metrics.Gauge Production = EKG.Gauge.Gauge
type instance Metrics.Distribution Production = EKG.Distribution.Distribution

instance Mockable Metrics.Metrics Production where

    {-# INLINABLE liftMockable #-}
    {-# SPECIALIZE INLINE liftMockable :: Metrics.Metrics Production t -> Production t #-}
    liftMockable term = case term of

        Metrics.NewGauge -> Production $ EKG.Gauge.new
        Metrics.IncGauge gauge -> Production $ EKG.Gauge.inc gauge
        Metrics.DecGauge gauge -> Production $ EKG.Gauge.dec gauge
        Metrics.SetGauge gauge val -> Production $ EKG.Gauge.set gauge val
        Metrics.ReadGauge gauge -> Production $ EKG.Gauge.read gauge

        Metrics.NewCounter -> Production $ EKG.Counter.new
        Metrics.IncCounter counter -> Production . EKG.Counter.inc $ counter
        Metrics.ReadCounter counter -> Production . EKG.Counter.read $ counter

        Metrics.NewDistribution -> Production $ EKG.Distribution.new
        Metrics.AddSample distr sample -> Production $ EKG.Distribution.add distr sample
        Metrics.ReadDistribution distr -> Production $ do
            stats <- EKG.Distribution.read distr
            return $ Metrics.Stats {
                  Metrics.mean = EKG.Distribution.mean stats
                , Metrics.variance = EKG.Distribution.variance stats
                , Metrics.count = EKG.Distribution.count stats
                , Metrics.sum = EKG.Distribution.sum stats
                , Metrics.min = EKG.Distribution.min stats
                , Metrics.max = EKG.Distribution.max stats
                }
