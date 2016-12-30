{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Mockable.Production where

import           Mockable.Class
import           Mockable.Concurrent
import           Mockable.SharedAtomic
import           Mockable.Channel
import           Mockable.Exception
import           Control.TimeWarp.Timed         (Microsecond, for, ms, hour)
import           Control.Monad                  (forever, void)
import           Control.Monad.Fix              (MonadFix)
import           Control.Monad.IO.Class         (MonadIO, liftIO)
import qualified Control.Exception              as Exception
import           Control.Exception.Base         (SomeException)
import qualified Control.Concurrent             as Conc
import qualified Control.Concurrent.MVar        as Conc
import qualified Control.Concurrent.STM         as Conc
import qualified Control.Concurrent.STM.TChan   as Conc
import           Control.Concurrent.STM.TVar    (newTVarIO, readTVarIO, writeTVar)
import           Data.Time.Clock.POSIX          (getPOSIXTime)

newtype Production t = Production {
    runProduction :: IO t
}

deriving instance Functor Production
deriving instance Applicative Production
deriving instance Monad Production
deriving instance MonadIO Production
deriving instance MonadFix Production

type instance ThreadId Production = Conc.ThreadId

instance Mockable Fork Production where
    liftMockable (Fork m) = Production $ Conc.forkIO (runProduction m)
    liftMockable (MyThreadId) = Production $ Conc.myThreadId
    liftMockable (KillThread tid) = Production $ Conc.killThread tid

instance Mockable Delay Production where
    liftMockable (Delay relativeToNow) = Production $ do
        cur <- runProduction $ curTime
        Conc.threadDelay $ fromIntegral $ relativeToNow cur - cur

instance Mockable SleepForever Production where
    liftMockable SleepForever = forever . wait $ for 24 hour

instance Mockable RepeatForever Production where
    liftMockable (RepeatForever period handler action) = do
        timer <- startTimer
        nextDelay <- liftIO $ newTVarIO Nothing
        void $ fork $
            let setNextDelay = liftIO . Conc.atomically . writeTVar nextDelay . Just
                action'      = action >> timer >>= \passed -> setNextDelay (period - passed)
                handler' e   = handler e >>= setNextDelay
            in action' `catch` handler'
        waitForRes nextDelay
      where
        startTimer = do
            start <- curTime
            return $ subtract start <$> curTime

        continue = repeatForever period handler action
        waitForRes nextDelay = do
            wait $ for 10 ms
            res <- liftIO $ readTVarIO nextDelay
            case res of
                Nothing -> waitForRes nextDelay
                Just t  -> wait (for t) >> continue

instance Mockable RunInUnboundThread Production where
    liftMockable (RunInUnboundThread m) = Production $
        Conc.runInUnboundThread (runProduction m)

type instance SharedAtomicT Production = Conc.MVar

instance Mockable SharedAtomic Production where
    liftMockable (NewSharedAtomic t) = Production $ Conc.newMVar t
    liftMockable (ModifySharedAtomic atomic f) = Production $ Conc.modifyMVar atomic (runProduction . f)

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

instance Mockable Throw Production where
    liftMockable (Throw e) = Production $ Exception.throwIO e

instance Mockable Catch Production where
    liftMockable (Catch action handler) = Production $
        runProduction action `Exception.catch` (runProduction . handler)

curTime :: Production Microsecond
curTime = liftIO $ round . ( * 1000000) <$> getPOSIXTime
