{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}

module Mockable.Production
       ( Production (..)
       ) where

import qualified Control.Concurrent       as Conc
import qualified Control.Concurrent.Async as Conc
import qualified Control.Concurrent.STM   as Conc
import qualified Control.Exception        as Exception
import           Control.Monad            (forever)
import           Control.Monad.Catch      (MonadCatch (..), MonadMask (..),
                                           MonadThrow (..))
import           Control.Monad.Fix        (MonadFix)
import           Control.Monad.IO.Class   (MonadIO)
import           Data.Time.Units          (Hour)
import           System.Wlog              (CanLog (..), HasLoggerName (..))

import           Mockable.Channel         (Channel (..), ChannelT)
import           Mockable.Class           (Mockable (..))
import           Mockable.Concurrent      (Async (..), Concurrently (..), Delay (..),
                                           Fork (..), Promise, RunInUnboundThread (..),
                                           ThreadId)
import           Mockable.CurrentTime     (CurrentTime (..), realTime)
import           Mockable.Exception       (Bracket (..), Catch (..), Throw (..))
import           Mockable.SharedAtomic    (SharedAtomic (..), SharedAtomicT)
import           Mockable.SharedExclusive (SharedExclusive (..), SharedExclusiveT)
import qualified Mockable.Metrics         as Metrics
import qualified System.Metrics.Distribution as EKG.Distribution
import qualified System.Metrics.Gauge     as EKG.Gauge
import qualified System.Metrics.Counter   as EKG.Counter
import           Serokell.Util.Concurrent as Serokell
import           Universum                (MonadFail (..))
import qualified Data.Sequence            as Seq

newtype Production t = Production
    { runProduction :: IO t
    } deriving (Functor, Applicative, Monad)

deriving instance MonadIO Production
deriving instance MonadFix Production
deriving instance MonadThrow Production
deriving instance MonadCatch Production
deriving instance CanLog Production

type instance ThreadId Production = Conc.ThreadId

instance Mockable Fork Production where
    liftMockable (Fork m)         = Production $ Conc.forkIO (runProduction m)
    liftMockable (MyThreadId)     = Production $ Conc.myThreadId
    liftMockable (KillThread tid) = Production $ Conc.killThread tid

instance Mockable Delay Production where
    liftMockable (Delay time) = Production $ Serokell.threadDelay time
    liftMockable SleepForever = Production $ forever $ Serokell.threadDelay (1 :: Hour)

instance Mockable RunInUnboundThread Production where
    liftMockable (RunInUnboundThread m) = Production $
        Conc.runInUnboundThread (runProduction m)

instance Mockable CurrentTime Production where
    liftMockable CurrentTime = realTime

type instance Promise Production = Conc.Async

instance Mockable Async Production where
    liftMockable (Async m)          = Production $ Conc.async (runProduction m)
    liftMockable (Wait promise)     = Production $ Conc.wait promise
    liftMockable (WaitAny promises) = Production $ Conc.waitAny promises
    liftMockable (Cancel promise)   = Production $ Conc.cancel promise

instance Mockable Concurrently Production where
    liftMockable (Concurrently a b) = Production $
        Conc.concurrently (runProduction a) (runProduction b)

type instance SharedAtomicT Production = Conc.MVar

instance Mockable SharedAtomic Production where
    liftMockable (NewSharedAtomic t)
        = Production $ Conc.newMVar t
    liftMockable (ModifySharedAtomic atomic f)
        = Production $ Conc.modifyMVar atomic (runProduction . f)

type instance SharedExclusiveT Production = Conc.MVar

instance Mockable SharedExclusive Production where
    liftMockable (NewSharedExclusive)
        = Production $ Conc.newEmptyMVar
    liftMockable (PutSharedExclusive var t)
        = Production $ Conc.putMVar var t
    liftMockable (TakeSharedExclusive var)
        = Production $ Conc.takeMVar var
    liftMockable (ModifySharedExclusive var f)
        = Production $ Conc.modifyMVar var (runProduction . f)

type instance ChannelT Production = Conc.TChan

instance Mockable Channel Production where
    liftMockable (NewChannel) = Production . Conc.atomically $ Conc.newTChan
    liftMockable (ReadChannel channel) = Production . Conc.atomically $ Conc.readTChan channel
    liftMockable (TryReadChannel channel) = Production . Conc.atomically $ Conc.tryReadTChan channel
    liftMockable (UnGetChannel channel t) = Production . Conc.atomically $ Conc.unGetTChan channel t
    liftMockable (WriteChannel channel t) = Production . Conc.atomically $ Conc.writeTChan channel t

instance Mockable Bracket Production where
    liftMockable (Bracket acquire release act) = Production $
        Exception.bracket (runProduction acquire) (runProduction . release) (runProduction . act)

    -- Base implementation doesn't give such a bracket so we have to do it
    -- ourselves.
    liftMockable (BracketWithException acquire release act) = Production $ mask $ \restore -> do
        a <- runProduction acquire
        r <- restore (runProduction (act a)) `catch` \e -> do
                 runProduction (release a (Just e))
                 Exception.throwIO e
        _ <- runProduction (release a Nothing)
        return r

instance Mockable Throw Production where
    liftMockable (Throw e) = Production $ Exception.throwIO e

instance Mockable Catch Production where
    liftMockable (Catch action handler) = Production $
        runProduction action `Exception.catch` (runProduction . handler)

newtype FailException = FailException String

deriving instance Show FailException
instance Exception.Exception FailException

instance MonadFail Production where
    fail = Production . Exception.throwIO . FailException

instance MonadMask Production where
    mask act = Production $ mask $
        \unmask -> runProduction $ act $ Production . unmask . runProduction
    uninterruptibleMask act = Production $ uninterruptibleMask $
        \unmask -> runProduction $ act $ Production . unmask . runProduction

instance HasLoggerName Production where
    getLoggerName = return "*production*"
    modifyLoggerName = const id

type instance Metrics.Counter Production = EKG.Counter.Counter
type instance Metrics.Gauge Production = EKG.Gauge.Gauge
type instance Metrics.Distribution Production = EKG.Distribution.Distribution

instance Mockable Metrics.Metrics Production where

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
