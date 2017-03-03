module Pos.Shutdown.Logic
       ( runIfNotShutdown
       , triggerShutdown
       , waitForWorkers
       ) where

import           Control.Concurrent.STM (readTBQueue, readTVar, writeTBQueue, writeTVar)
import           System.Wlog            (WithLogger, logDebug, logInfo)
import           Universum

import           Pos.Shutdown.Class     (MonadShutdownMem (..))
import           Pos.Shutdown.Types     (ShutdownContext (..))

runIfNotShutdown
    :: (MonadIO m, MonadShutdownMem m, WithLogger m)
    => m () -> m ()
runIfNotShutdown = ifM isShutdown notifyQueue
  where
    isShutdown = _shdnIsTriggered <$> askShutdownMem >>= atomically . readTVar
    notifyQueue = do
        logDebug "runIfNotShutdown: shutdown case triggered"
        _shdnNotifyQueue <$> askShutdownMem >>=
            atomically . flip writeTBQueue ()

triggerShutdown
    :: (MonadIO m, WithLogger m, MonadShutdownMem m)
    => m ()
triggerShutdown = do
    logInfo "NODE SHUTDOWN TRIGGERED, WAITING FOR WORKERS TO TERMINATE"
    _shdnIsTriggered <$> askShutdownMem >>= atomically . flip writeTVar True

waitForWorkers
    :: (MonadIO m, MonadShutdownMem m)
    => Int -> m ()
waitForWorkers 0 = pass
waitForWorkers n = do
    _shdnNotifyQueue <$> askShutdownMem >>= atomically . readTBQueue
    waitForWorkers (n - 1)
