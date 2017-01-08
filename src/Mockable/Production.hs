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
import           Control.TimeWarp.Timed   (for, hour)
import           System.Wlog              (CanLog (..))

import           Mockable.Channel         (Channel (..), ChannelT)
import           Mockable.Class           (Mockable (..))
import           Mockable.Concurrent      (Async (..), Concurrently (..), Delay (..),
                                           Fork (..), Promise, RunInUnboundThread (..),
                                           ThreadId, delay)
import           Mockable.CurrentTime     (CurrentTime (..), realTime)
import           Mockable.Exception       (Bracket (..), Catch (..), Throw (..))
import           Mockable.SharedAtomic    (SharedAtomic (..), SharedAtomicT)
import           Universum                (MonadFail (..))

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
    liftMockable (Delay relativeToNow) = Production $ do
        now <- realTime
        Conc.threadDelay $ fromIntegral $ relativeToNow now - now
    liftMockable SleepForever = forever $ delay $ for 24 hour

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
