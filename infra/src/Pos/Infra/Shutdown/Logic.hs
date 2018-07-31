module Pos.Infra.Shutdown.Logic
       ( triggerShutdown
       , waitForShutdown
       ) where

import           Universum

import           Control.Concurrent.STM (check, readTVar, writeTVar)

import           Pos.Infra.Shutdown.Class (HasShutdownContext (..))
import           Pos.Infra.Shutdown.Types (ShutdownContext (..),
                     shdnIsTriggered)
import           Pos.Util.Trace.Named (TraceNamed, logInfo)


triggerShutdown
    :: (MonadIO m, MonadReader ctx m, HasShutdownContext ctx)
    => TraceNamed m
    -> m ()
triggerShutdown logTrace = do
    logInfo logTrace "NODE SHUTDOWN TRIGGERED, WAITING FOR WORKERS TO TERMINATE"
    view (shutdownContext . shdnIsTriggered) >>= atomically . flip writeTVar True

-- | Wait for the shutdown var to be true.
waitForShutdown :: ShutdownContext -> IO ()
waitForShutdown (ShutdownContext v) = atomically (readTVar v >>= check)
