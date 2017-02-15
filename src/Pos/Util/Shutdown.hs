-- | Utility functions for graceful shutdown of workers

module Pos.Util.Shutdown
       ( ifNotShutdown
       , triggerShutdown
       , waitForWorkers
       ) where

import           Control.Concurrent.STM (readTBQueue, readTVar, writeTBQueue, writeTVar)
import           System.Wlog            (WithLogger, logDebug, logInfo)
import           Universum

import           Pos.Context.Class      (WithNodeContext, getNodeContext)
import           Pos.Context.Context    (ncShutdownFlag, ncShutdownNotifyQueue)

ifNotShutdown
    :: (MonadIO m, WithNodeContext ssc m, WithLogger m)
    => m () -> m ()
ifNotShutdown = ifM isShutdown notifyQueue
  where
    isShutdown = ncShutdownFlag <$> getNodeContext >>= atomically . readTVar
    notifyQueue = do
        logDebug "ifNotShutdown: shutdown case triggered"
        ncShutdownNotifyQueue <$> getNodeContext >>=
            atomically . flip writeTBQueue ()

triggerShutdown
    :: (MonadIO m, WithLogger m, WithNodeContext ssc m)
    => m ()
triggerShutdown = do
    logInfo "NODE SHUTDOWN TRIGGERED, WAITING FOR WORKERS TO TERMINATE"
    ncShutdownFlag <$> getNodeContext >>= atomically . flip writeTVar True

waitForWorkers
    :: (MonadIO m, WithNodeContext ssc m)
    => Int -> m ()
waitForWorkers 0 = pass
waitForWorkers n = do
    ncShutdownNotifyQueue <$> getNodeContext >>= atomically . readTBQueue
    waitForWorkers (n - 1)
