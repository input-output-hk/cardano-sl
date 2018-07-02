{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}

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
import           Crypto.Random.Entropy (getEntropy)
import           Data.Time.Units (toMicroseconds)
import qualified GHC.IO as GHC
import qualified System.Metrics.Counter as EKG.Counter
import qualified System.Metrics.Distribution as EKG.Distribution
import qualified System.Metrics.Gauge as EKG.Gauge

import           Control.Monad.Base (MonadBase (..))
import           Control.Monad.Trans.Control (MonadBaseControl (..))
import           Control.Monad.Trans.Reader (ReaderT (..), mapReaderT)
import           Mockable.Channel (Channel (..), ChannelT)
import           Mockable.Class (Mockable (..))
import           Mockable.Concurrent (Async (..), Concurrently (..), Delay (..),
                     Fork (..), LowLevelAsync (..), MyThreadId (..), Promise,
                     RunInUnboundThread (..), ThreadId)
import           Mockable.CurrentTime (CurrentTime (..), realTime)
import qualified Mockable.Metrics as Metrics
import           Mockable.SharedAtomic (SharedAtomic (..), SharedAtomicT)
import           Mockable.SharedExclusive (SharedExclusive (..),
                     SharedExclusiveT)

import qualified Katip as K
import qualified Katip.Monadic as KM
import qualified Pos.Util.Log as Log

newtype Production t = Production
    { runProduction :: Log.LogContextT IO t
    } deriving (Functor, Applicative, Monad)

deriving instance MonadIO Production
deriving instance MonadUnliftIO Production
deriving instance MonadFix Production
deriving instance MonadThrow Production
deriving instance MonadCatch Production
deriving instance K.Katip Production
deriving instance K.KatipContext Production

instance Rand.MonadRandom Production where
    getRandomBytes = Production . KM.KatipContextT . ReaderT . const . getEntropy

type instance ThreadId Production = Conc.ThreadId

instance Mockable Fork Production where
    {-# INLINABLE liftMockable #-}
    {-# SPECIALIZE INLINE liftMockable :: Fork Production t -> Production t #-}
    liftMockable (Fork m)        = Production $ KM.KatipContextT $ mapReaderT
                                     Conc.forkIO $ KM.unKatipContextT (runProduction m)
    liftMockable (ThrowTo tid e) = Production $ KM.KatipContextT $ ReaderT $ const $ Conc.throwTo tid e

instance Mockable Delay Production where
    {-# INLINABLE liftMockable #-}
    {-# SPECIALIZE INLINE liftMockable :: Delay Production t -> Production t #-}
    liftMockable (Delay time) = Production $
        -- toMicroseconds :: TimeUnit t => t -> Integer
        -- then we cast to an Int. Hopefully it fits!
        KM.KatipContextT $ ReaderT $ const $ Conc.threadDelay (fromIntegral (toMicroseconds time))

instance Mockable MyThreadId Production where
    {-# INLINABLE liftMockable #-}
    {-# SPECIALIZE INLINE liftMockable :: MyThreadId Production t -> Production t #-}
    liftMockable (MyThreadId) = Production $ KM.KatipContextT $ ReaderT $ const $ Conc.myThreadId

instance Mockable RunInUnboundThread Production where
    {-# INLINABLE liftMockable #-}
    {-# SPECIALIZE INLINE liftMockable :: RunInUnboundThread Production t -> Production t #-}
    liftMockable (RunInUnboundThread m) = Production $ KM.KatipContextT $ mapReaderT
        Conc.runInUnboundThread $ KM.unKatipContextT (runProduction m)

instance Mockable CurrentTime Production where
    {-# INLINABLE liftMockable #-}
    {-# SPECIALIZE INLINE liftMockable :: CurrentTime Production t -> Production t #-}
    liftMockable CurrentTime = realTime

type instance Promise Production = Conc.Async

instance Mockable Async Production where
    {-# INLINABLE liftMockable #-}
    {-# SPECIALIZE INLINE liftMockable :: Async Production t -> Production t #-}
    liftMockable (WithAsync m k)        = do
                                            le  <- K.getLogEnv
                                            ctx <- K.getKatipContext
                                            ns  <- K.getKatipNamespace
                                            let m' = K.runKatipContextT le ctx ns (runProduction m)
                                                k' = \p -> K.runKatipContextT le ctx ns ((runProduction . k) p)
                                            Production $ KM.KatipContextT $ ReaderT $ const $
                                                Conc.withAsync m' k'
    liftMockable (AsyncThreadId p)      = Production $ KM.KatipContextT $ ReaderT $ const $
                                            return (Conc.asyncThreadId p)
    liftMockable (Race a b)             = do
                                            le  <- K.getLogEnv
                                            ctx <- K.getKatipContext
                                            ns  <- K.getKatipNamespace
                                            let a' = K.runKatipContextT le ctx ns (runProduction a)
                                                b' = K.runKatipContextT le ctx ns (runProduction b)
                                            Production $ KM.KatipContextT $ ReaderT $ const $
                                                Conc.race a' b'
    liftMockable (UnsafeUnmask act)     = Production $ KM.KatipContextT $ mapReaderT
                                            GHC.unsafeUnmask $ KM.unKatipContextT (runProduction act)

instance Mockable LowLevelAsync Production where
    {-# INLINABLE liftMockable #-}
    {-# SPECIALIZE INLINE liftMockable :: LowLevelAsync Production t -> Production t #-}
    liftMockable (Async m)              = Production $ KM.KatipContextT $ mapReaderT
                                            Conc.async $ KM.unKatipContextT (runProduction m)
    liftMockable (Link p)               = Production $ KM.KatipContextT $ ReaderT $ const $
                                            Conc.link p
    liftMockable (Wait promise)         = Production $ KM.KatipContextT $ ReaderT $ const $
                                            Conc.wait promise
    liftMockable (WaitAny promises)     = Production $ KM.KatipContextT $ ReaderT $ const $
                                            Conc.waitAny promises
    liftMockable (CancelWith promise e) = Production $ KM.KatipContextT $ ReaderT $ const $
                                            Conc.cancelWith promise e

instance Mockable Concurrently Production where
    {-# INLINABLE liftMockable #-}
    {-# SPECIALIZE INLINE liftMockable :: Concurrently Production t -> Production t #-}
    liftMockable (Concurrently a b) = do
        le  <- K.getLogEnv
        ctx <- K.getKatipContext
        ns  <- K.getKatipNamespace
        let a' = K.runKatipContextT le ctx ns (runProduction a)
            b' = K.runKatipContextT le ctx ns (runProduction b)
        Production $ KM.KatipContextT $ ReaderT $ const $
            Conc.concurrently a' b'

type instance SharedAtomicT Production = Conc.MVar

instance Mockable SharedAtomic Production where
    {-# INLINABLE liftMockable #-}
    {-# SPECIALIZE INLINE liftMockable :: SharedAtomic Production t -> Production t #-}
    liftMockable (NewSharedAtomic t)
        = Production $ KM.KatipContextT $ ReaderT $ const $ Conc.newMVar t
    liftMockable (ModifySharedAtomic atomic f)
        = do
            le  <- K.getLogEnv
            ctx <- K.getKatipContext
            ns  <- K.getKatipNamespace
            Production $ KM.KatipContextT $ ReaderT $ const $ Conc.modifyMVar atomic $ \p ->
                K.runKatipContextT le ctx ns ((runProduction . f) p)
    liftMockable (ReadSharedAtomic atomic)
        = Production $ KM.KatipContextT $ ReaderT $ const $ Conc.readMVar atomic

type instance SharedExclusiveT Production = Conc.MVar

instance Mockable SharedExclusive Production where
    {-# INLINABLE liftMockable #-}
    {-# SPECIALIZE INLINE liftMockable :: SharedExclusive Production t -> Production t #-}
    liftMockable (NewSharedExclusive)
        = Production $ KM.KatipContextT $ ReaderT $ const Conc.newEmptyMVar
    liftMockable (PutSharedExclusive var t)
        = Production $ KM.KatipContextT $ ReaderT $ const $ Conc.putMVar var t
    liftMockable (TakeSharedExclusive var)
        = Production $ KM.KatipContextT $ ReaderT $ const $ Conc.takeMVar var
    liftMockable (ModifySharedExclusive var f)
        = do
            le  <- K.getLogEnv
            ctx <- K.getKatipContext
            ns  <- K.getKatipNamespace
            Production $ KM.KatipContextT $ ReaderT $ const $ Conc.modifyMVar var $ \p ->
                K.runKatipContextT le ctx ns ((runProduction . f) p)
    liftMockable (TryPutSharedExclusive var t)
        = Production $ KM.KatipContextT $ ReaderT $ const $ Conc.tryPutMVar var t

type instance ChannelT Production = Conc.TChan

instance Mockable Channel Production where
    {-# INLINABLE liftMockable #-}
    {-# SPECIALIZE INLINE liftMockable :: Channel Production t -> Production t #-}
    liftMockable (NewChannel) = Production $ KM.KatipContextT $ ReaderT $ const $
        Conc.atomically Conc.newTChan
    liftMockable (ReadChannel channel) = Production $ KM.KatipContextT $ ReaderT $ const $
        Conc.atomically $ Conc.readTChan channel
    liftMockable (TryReadChannel channel) = Production $ KM.KatipContextT $ ReaderT $ const $
        Conc.atomically $ Conc.tryReadTChan channel
    liftMockable (UnGetChannel channel t) = Production $ KM.KatipContextT $ ReaderT $ const $
        Conc.atomically $ Conc.unGetTChan channel t
    liftMockable (WriteChannel channel t) = Production $ KM.KatipContextT $ ReaderT $ const $
        Conc.atomically $ Conc.writeTChan channel t

newtype FailException = FailException String

deriving instance Show FailException
instance Exception.Exception FailException

instance MonadMask Production where
    mask act = Production $ mask $
        \unmask -> runProduction $ act $ Production . unmask . runProduction
    uninterruptibleMask act = Production $ uninterruptibleMask $
        \unmask -> runProduction $ act $ Production . unmask . runProduction

{- TODO
instance HasLoggerName Production where
    askLoggerName = return "*production*"
    modifyLoggerName = const id
-}

instance MonadBase IO Production where
    liftBase = Production . KM.KatipContextT . ReaderT . const

-- instance MonadBase (Log.LogContextT IO) Production where
--     liftBase = Production

instance MonadBaseControl IO Production where
    type StM Production a = a
    liftBaseWith f = Production $ liftBaseWith $ \q -> f (q . runProduction)
    restoreM = Production . pure

-- instance MonadBaseControl (Log.LogContextT IO) Production where
--     type StM Production a = a
--     liftBaseWith f = do
--         le  <- K.getLogEnv
--         ctx <- K.getKatipContext
--         ns  <- K.getKatipNamespace
--         Production $ liftBaseWith $ \q -> f (q . runProduction)
--     restoreM = Production . pure

type instance Metrics.Counter Production = EKG.Counter.Counter
type instance Metrics.Gauge Production = EKG.Gauge.Gauge
type instance Metrics.Distribution Production = EKG.Distribution.Distribution

instance Mockable Metrics.Metrics Production where

    {-# INLINABLE liftMockable #-}
    {-# SPECIALIZE INLINE liftMockable :: Metrics.Metrics Production t -> Production t #-}
    liftMockable term = case term of

        Metrics.NewGauge -> Production $ KM.KatipContextT $ ReaderT $ const $ EKG.Gauge.new
        Metrics.IncGauge gauge -> Production $ KM.KatipContextT $ ReaderT $ const $ EKG.Gauge.inc gauge
        Metrics.DecGauge gauge -> Production $ KM.KatipContextT $ ReaderT $ const $ EKG.Gauge.dec gauge
        Metrics.SetGauge gauge val -> Production $ KM.KatipContextT $ ReaderT $ const $
            EKG.Gauge.set gauge val
        Metrics.ReadGauge gauge -> Production $ KM.KatipContextT $ ReaderT $ const $
            EKG.Gauge.read gauge

        Metrics.NewCounter -> Production $ KM.KatipContextT $ ReaderT $ const $
            EKG.Counter.new
        Metrics.IncCounter counter -> Production $ KM.KatipContextT $ ReaderT $ const $
            EKG.Counter.inc counter
        Metrics.ReadCounter counter -> Production $ KM.KatipContextT $ ReaderT $ const $
            EKG.Counter.read $ counter

        Metrics.NewDistribution -> Production $ KM.KatipContextT $ ReaderT $ const $
            EKG.Distribution.new
        Metrics.AddSample distr sample -> Production $ KM.KatipContextT $ ReaderT $ const $
            EKG.Distribution.add distr sample
        Metrics.ReadDistribution distr -> Production $ KM.KatipContextT $ ReaderT $ const $ do
            stats <- EKG.Distribution.read distr
            return $ Metrics.Stats {
                  Metrics.mean = EKG.Distribution.mean stats
                , Metrics.variance = EKG.Distribution.variance stats
                , Metrics.count = EKG.Distribution.count stats
                , Metrics.sum = EKG.Distribution.sum stats
                , Metrics.min = EKG.Distribution.min stats
                , Metrics.max = EKG.Distribution.max stats
                }
