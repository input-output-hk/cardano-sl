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
import           Control.Monad.Fix        (MonadFix)
import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Data.Time.Clock.POSIX    (getPOSIXTime)
import           Data.Time.Units          (Microsecond)
import qualified Serokell.Util.Concurrent as Serokell
import           System.Wlog              (CanLog (..), HasLoggerName (..))

import Mockable.Channel      (Channel (..), ChannelT)
import Mockable.Class        (Mockable (..))
import Mockable.Concurrent   (Async (..), Concurrently (..), CurrentTime (..), Delay (..),
                              Fork (..), Promise, RunInUnboundThread (..), ThreadId)
import Mockable.Exception    (Bracket (..), Catch (..), Throw (..))
import Mockable.SharedAtomic (SharedAtomic (..), SharedAtomicT)

newtype Production t = Production
    { runProduction :: IO t
    }

deriving instance Functor Production
deriving instance Applicative Production
deriving instance Monad Production
deriving instance MonadIO Production
deriving instance MonadFix Production

type instance ThreadId Production = Conc.ThreadId

instance Mockable Fork Production where
    liftMockable (Fork m)         = Production $ Conc.forkIO (runProduction m)
    liftMockable (MyThreadId)     = Production $ Conc.myThreadId
    liftMockable (KillThread tid) = Production $ Conc.killThread tid

instance Mockable Delay Production where
    liftMockable (Delay time) = Production $ Serokell.threadDelay time
    liftMockable SleepForever = Production $ Conc.atomically Conc.retry

instance Mockable RunInUnboundThread Production where
    liftMockable (RunInUnboundThread m) = Production $
        Conc.runInUnboundThread (runProduction m)

instance Mockable CurrentTime Production where
    liftMockable CurrentTime = curTime

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

-- * Temporal instances, till we get proper instances of `Mockable` for
-- `LoggerNameBox Production`

instance HasLoggerName Production where
    getLoggerName = return "ntp-example"
    modifyLoggerName = const id

instance CanLog Production where
    dispatchMessage n sv text = Production $ dispatchMessage n sv text

curTime :: Production Microsecond
curTime = liftIO $ round . ( * 1000000) <$> getPOSIXTime
