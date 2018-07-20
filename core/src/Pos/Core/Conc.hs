{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Temporary holding bin for useful functions defined in `Pos.Core.Mockable`.
-- Since we are removing the `Mockable` class, some of the functions defined
-- there will move here.
module Pos.Core.Conc
    ( currentTime
    , currentTimeUnits
    , delay
    , withAsync
    , withAsyncWithUnmask
    , race
    , timeout
    , concurrently
    , mapConcurrently
    , forConcurrently
    , async
    , cancel
    , wait
    , modifySharedAtomic
    , newSharedAtomic
    ) where

import           Universum hiding (newMVar)

import qualified Control.Concurrent as Conc
import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Time.Units (Microsecond, TimeUnit, convertUnit,
                     toMicroseconds)
import           System.Wlog (HasLoggerName (..))
import           UnliftIO (MonadUnliftIO)
import           UnliftIO.Async (async, cancel, concurrently, forConcurrently,
                     mapConcurrently, race, wait, withAsync,
                     withAsyncWithUnmask)
import           UnliftIO.MVar (modifyMVar, newMVar)

import           Pos.Util (realTime)

currentTime :: MonadIO m => m Microsecond
currentTime = liftIO realTime

currentTimeUnits :: (TimeUnit t, MonadIO m) => m t
currentTimeUnits = convertUnit <$> currentTime

-- toMicroseconds :: TimeUnit t => t -> Integer
-- then we cast to an Int. Hopefully it fits!
delay :: (TimeUnit t, MonadIO m)
         => t
         -> m ()
delay time = liftIO (Conc.threadDelay (fromIntegral (toMicroseconds time)))

-- | This function is analogous to `System.Timeout.timeout`, it's
-- based on `Race` and `Delay`.
-- TODO mhueschen - make sure this is equivalent to the old method
timeout :: (TimeUnit t, MonadUnliftIO m) => t -> m a -> m (Maybe a)
timeout t ma = rightToMaybe <$> race (delay t) ma


newSharedAtomic :: MonadIO m => a -> m (MVar a)
newSharedAtomic = newMVar

modifySharedAtomic :: MonadUnliftIO m => MVar a -> (a -> m (a, b)) -> m b
modifySharedAtomic = modifyMVar

instance HasLoggerName IO where
    askLoggerName = return "*production*"
    modifyLoggerName = const id
