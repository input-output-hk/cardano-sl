module Pos.Shutdown.Logic
       ( runIfNotShutdown
       , triggerShutdown
       , waitForWorkers
       ) where

import           Universum

import           Control.Concurrent.STM (readTBQueue, readTVar, writeTBQueue, writeTVar)
import           System.Wlog            (WithLogger, logDebug, logInfo)

import           Pos.Shutdown.Class     (HasShutdownContext (..))
import           Pos.Shutdown.Types     (shdnIsTriggered, shdnNotifyQueue)

runIfNotShutdown
    :: (MonadIO m, MonadReader ctx m, HasShutdownContext ctx, WithLogger m)
    => m () -> m ()
runIfNotShutdown = ifM isShutdown notifyQueue
  where
    isShutdown = view (shutdownContext . shdnIsTriggered) >>= atomically . readTVar
    notifyQueue = do
        logDebug "runIfNotShutdown: shutdown case triggered"
        view (shutdownContext . shdnNotifyQueue) >>=
            atomically . flip writeTBQueue ()

triggerShutdown
    :: (MonadIO m, MonadReader ctx m, WithLogger m, HasShutdownContext ctx)
    => m ()
triggerShutdown = do
    logInfo "NODE SHUTDOWN TRIGGERED, WAITING FOR WORKERS TO TERMINATE"
    view (shutdownContext . shdnIsTriggered) >>= atomically . flip writeTVar True

waitForWorkers
    :: (MonadIO m, MonadReader ctx m, HasShutdownContext ctx)
    => Int -> m ()
waitForWorkers 0 = pass
waitForWorkers n = do
    view (shutdownContext . shdnNotifyQueue) >>= atomically . readTBQueue
    waitForWorkers (n - 1)
