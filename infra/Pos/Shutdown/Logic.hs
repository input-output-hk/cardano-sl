module Pos.Shutdown.Logic
       ( triggerShutdown
       , waitForShutdown
       ) where

import           Universum

import           Control.Concurrent.STM (check, readTVar, writeTVar)
import           System.Wlog (WithLogger, logInfo)

import           Pos.Shutdown.Class (HasShutdownContext (..))
import           Pos.Shutdown.Types (ShutdownContext (..), shdnIsTriggered)

triggerShutdown
    :: (MonadIO m, MonadReader ctx m, WithLogger m, HasShutdownContext ctx)
    => m ()
triggerShutdown = do
    logInfo "NODE SHUTDOWN TRIGGERED, WAITING FOR WORKERS TO TERMINATE"
    view (shutdownContext . shdnIsTriggered) >>= atomically . flip writeTVar True

-- | Wait for the shutdown var to be true.
waitForShutdown :: ShutdownContext -> IO ()
waitForShutdown (ShutdownContext v) = atomically (readTVar v >>= check)
