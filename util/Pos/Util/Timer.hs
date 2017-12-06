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
    , waitTimer
    , startTimer
    ) where

import           Control.Concurrent.STM (newTVar, readTVar, registerDelay, retry)
import           Universum

data Timer = Timer
  { delayDuration  :: !Int
  , delaySemaphore :: !(TVar (TVar Bool))
  }

-- | Create a new, inactive Timer given the number of microseconds. In order to
-- activate it 'startTimer' needs to be called.
newTimer :: MonadIO m => Int -> m Timer
newTimer n
    | n < 0     = error "newTimer: negative duration"
    | otherwise = Timer n <$> atomically (newTVar True >>= newTVar)

-- | Wait for the duration associated with the Timer that passed since the last
-- time 'startTimer' was called.
waitTimer :: Timer -> STM ()
waitTimer Timer{..} = do
  done <- readTVar =<< readTVar delaySemaphore
  unless done retry

-- Start the timer. If the function is called before @t + n@, where @t@ is the
-- time startTimer was called for the last time and @n@ is the number of
-- microseconds associated with the Timer, the timer is restarted.
startTimer :: MonadIO m => Timer -> m ()
startTimer Timer{..} = liftIO (registerDelay delayDuration)
    >>= atomically . writeTVar delaySemaphore
