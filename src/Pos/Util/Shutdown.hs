-- | Utility functions for graceful shutdown of workers

module Pos.Util.Shutdown
       ( ifNotShutdown
       , triggerShutdown
       , waitForWorkers
       ) where

import           Control.Concurrent.STM (readTBQueue, readTVar, writeTBQueue, writeTVar)
import           System.Wlog            (WithLogger, logInfo)
import           Universum

import           Pos.Context.Class      (WithNodeContext, getNodeContext)
import           Pos.Context.Context    (ncShutdownFlag, ncShutdownNotifyQueue)

ifNotShutdown
    :: (MonadIO m, WithNodeContext ssc m)
    => m () -> m ()
ifNotShutdown = ifM isShutdown notifyQueue
  where isShutdown = ncShutdownFlag <$> getNodeContext >>=
                     atomically . readTVar
        notifyQueue = ncShutdownNotifyQueue <$> getNodeContext >>=
                      atomically . flip writeTBQueue ()

triggerShutdown
    :: (MonadIO m, WithLogger m, WithNodeContext ssc m)
    => m ()
triggerShutdown = do
    ncShutdownFlag <$> getNodeContext >>= atomically . flip writeTVar True
    logInfo "NODE SHUTDOWN TRIGGERED, WAITING FOR WORKERS TO TERMINATE"

waitForWorkers
    :: (MonadIO m, WithNodeContext ssc m)
    => Int -> m ()
waitForWorkers 0 = return ()
waitForWorkers n =
    ncShutdownNotifyQueue <$> getNodeContext >>= atomically . readTBQueue >>
    waitForWorkers (n - 1)
