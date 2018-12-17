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
-- Use Integer to avoid potential Int overflow.
delay :: (TimeUnit t, MonadIO m)
         => t
         -> m ()
delay =
    liftIO . sleep . toMicroseconds
  where
    sleep :: Integer -> IO ()
    sleep time = do
        let maxWait = min time $ toInteger (maxBound :: Int)
        Conc.threadDelay (fromInteger maxWait)
        when (maxWait /= time) $ sleep (time - maxWait)


-- | This function is analogous to `System.Timeout.timeout`. It's
-- based on `race` and `delay`.
timeout :: (TimeUnit t, MonadUnliftIO m) => t -> m a -> m (Maybe a)
timeout t ma = rightToMaybe <$> race (delay t) ma


newSharedAtomic :: MonadIO m => a -> m (MVar a)
newSharedAtomic = newMVar

modifySharedAtomic :: MonadUnliftIO m => MVar a -> (a -> m (a, b)) -> m b
modifySharedAtomic = modifyMVar
