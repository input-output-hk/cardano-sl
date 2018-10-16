{-# LANGUAGE RecordWildCards #-}

module Pos.Infra.Shutdown.Logic
       ( triggerShutdown
       , waitForShutdown
       ) where

import           Universum

import           Control.Concurrent.STM (check, readTVar, writeTVar)

import           Pos.Infra.InjectFail (FInject(..), testLogFInject)
import           Pos.Infra.Shutdown.Class (HasShutdownContext (..))
import           Pos.Infra.Shutdown.Types (ShutdownContext (..),
                     shdnIsTriggered, shdnFInjects)
import           Pos.Util.Wlog (WithLogger, logInfo)

triggerShutdown
    :: (MonadIO m, MonadReader ctx m, WithLogger m, HasShutdownContext ctx)
    => m ()
triggerShutdown = do
    shutCtx <- view shutdownContext
    doFail <- testLogFInject (shutCtx ^. shdnFInjects) FInjIgnoreShutdown
    unless doFail $ do
      logInfo "NODE SHUTDOWN TRIGGERED, WAITING FOR WORKERS TO TERMINATE"
      view (shutdownContext . shdnIsTriggered) >>= atomically . flip writeTVar True

-- | Wait for the shutdown var to be true.
waitForShutdown :: ShutdownContext -> IO ()
waitForShutdown ShutdownContext{..} = atomically (readTVar _shdnIsTriggered >>= check)
