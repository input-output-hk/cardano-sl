-- | Restartable, STM-based dynamic timer build on top of `Pos.Util.Timer.Timer`.
module Pos.Util.Timer
  ( Timer
  , newTimer
  , waitTimer
  , startTimer
  ) where

import           Data.Time.Units (TimeUnit, toMicroseconds)
import           Control.Concurrent.STM (readTVar, registerDelay, retry)
import           Universum

newtype Timer = Timer { timerSemaphore :: TVar (TVar Bool) }

-- | Construct new `Timer`.
newTimer :: MonadIO m => m Timer
newTimer = Timer <$> (newTVarIO True >>= newTVarIO)

-- | Wait for the duration associated with the Timer that passed since the last
-- time `startTimer` was called.
waitTimer :: Timer -> STM ()
waitTimer Timer{..} = do
  done <- readTVar =<< readTVar timerSemaphore
  unless done retry

-- | Set the time duration of the underlying timer and start the underlying
-- timer.
startTimer :: (MonadIO m, TimeUnit t) => t -> Timer -> m ()
startTimer time Timer{..} =
      (liftIO . registerDelay . fromIntegral . toMicroseconds $ time)
  >>= atomically . writeTVar timerSemaphore
