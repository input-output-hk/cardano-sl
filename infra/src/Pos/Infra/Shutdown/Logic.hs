module Pos.Infra.Shutdown.Logic
       ( triggerShutdown
       , triggerShutdown'
       , waitForShutdown
       ) where

import           System.Exit (ExitCode (ExitFailure))
import           Universum

import           Control.Concurrent.STM (readTVar, retry, writeTVar)

import           Pos.Infra.InjectFail (FInject (..), testLogFInject)
import           Pos.Infra.Shutdown.Class (HasShutdownContext (..))
import           Pos.Infra.Shutdown.Types (ShutdownContext (..), shdnFInjects,
                     shdnIsTriggered)
import           Pos.Util.Wlog (WithLogger, logInfo)

{-# INLINE triggerShutdown' #-}
triggerShutdown'
    :: (MonadIO m, MonadReader ctx m, WithLogger m, HasShutdownContext ctx)
    => ExitCode -> m ()
triggerShutdown' exitcode = do
    shutCtx <- view shutdownContext
    doFail <- liftIO $ testLogFInject (shutCtx ^. shdnFInjects) FInjIgnoreShutdown
    applyWrongCode <- liftIO $ testLogFInject (shutCtx ^. shdnFInjects) FInjApplyUpdateWrongExitCode
    let
      realCode = if applyWrongCode
        then ExitFailure 42 -- inject wrong exit code
        else exitcode
    unless doFail $ do
      logInfo "NODE SHUTDOWN TRIGGERED, WAITING FOR WORKERS TO TERMINATE"
      view (shutdownContext . shdnIsTriggered) >>= atomically . flip writeTVar (Just realCode)

{-# INLINE triggerShutdown #-}
triggerShutdown
    :: (MonadIO m, MonadReader ctx m, WithLogger m, HasShutdownContext ctx)
    => m ()
triggerShutdown = triggerShutdown' $ ExitFailure 20 -- special exit code to indicate an update

-- | Wait for the shutdown var to be true.
waitForShutdown :: ShutdownContext -> IO ExitCode
waitForShutdown (ShutdownContext shutdownTriggered _) = do
  let
    go :: STM ExitCode
    go = do
      res <- readTVar shutdownTriggered
      case res of
        Nothing -> retry
        Just a  -> pure a
  atomically go
