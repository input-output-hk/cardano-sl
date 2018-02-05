-- | Restartable, STM-based timer for waiting a given number of microseconds.
--
-- Consider the following code:
-- @
--   main :: IO ()
--   main = do
--     delay <- newTimer $ 5 * 1000000
--     tid <- forkIO . forever $ do
--       startTimer delay
--       atomically $ waitTimer delay
--       putStrLn $ "Timer ended"
--     (`finally` killThread tid) . forever $ do
--       _ <- getLine
--       startTimer delay
-- @
--
-- It will print "Timer ended" every 5 seconds after there was no input for 5
-- seconds.
module Pos.Util.Timer
    ( Timer
    , newTimer
    , setTimerDuration
    , waitTimer
    , startTimer
    ) where

import           Data.Time.Units (Microsecond)
import           Control.Concurrent (modifyMVar_)
import           Control.Concurrent.STM (readTVar, registerDelay, retry)
import           Universum

data Timer = Timer
  { timerDuration   :: !(MVar Microsecond)
  , timerSemaphore :: !(TVar (TVar Bool))
  }

-- | Create a new, inactive Timer given the number of microseconds. In order to
-- activate it 'startTimer' needs to be called.
newTimer :: MonadIO m => Microsecond -> m Timer
newTimer n = Timer <$> newMVar n <*> (newTVarIO True >>= newTVarIO)

-- | Set the duration of a timer to a specified number of microseconds. Note
-- that if the timer is already started, calling this function wont't restart
-- it.
setTimerDuration :: MonadIO m => Timer -> Microsecond -> m ()
setTimerDuration Timer{..} n = liftIO $ modifyMVar_ timerDuration $ \_ -> return $! n

-- | Wait for the duration associated with the Timer that passed since the last
-- time 'startTimer' was called.
waitTimer :: Timer -> STM ()
waitTimer Timer{..} = do
  done <- readTVar =<< readTVar timerSemaphore
  unless done retry

-- Start the timer. If the function is called before @t + n@, where @t@ is the
-- time startTimer was called for the last time and @n@ is the number of
-- microseconds associated with the Timer, the timer is restarted.
startTimer :: MonadIO m => Timer -> m ()
startTimer Timer{..} = liftIO (registerDelay . fromIntegral =<< readMVar timerDuration)
    >>= atomically . writeTVar timerSemaphore
