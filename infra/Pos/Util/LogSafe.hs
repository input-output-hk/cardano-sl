-- | Safe/secure logging

module Pos.Util.LogSafe
       ( SecureLogWrapped(..)
       , logDebugS
       , logInfoS
       , logNoticeS
       , logWarningS
       , logErrorS
       ) where

import           Universum

import           Control.Monad.Trans (MonadTrans)
import           Data.List           (isSuffixOf)
import           System.Wlog         (CanLog (..), HasLoggerName (..), Severity (..),
                                      loggerName)
import           System.Wlog.Handler (LogHandlerTag (HandlerFilelike))
import           System.Wlog.Logger  (logMCond)

newtype SecureLogWrapped m a = SecureLogWrapped
    { getSecureLogWrapped :: m a
    } deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans SecureLogWrapped where
    lift = SecureLogWrapped

instance (MonadIO m) => CanLog (SecureLogWrapped m) where
    dispatchMessage
        (loggerName      -> name)
        severity
        msg =
      let acceptable (HandlerFilelike p) = not $ ".pub" `isSuffixOf` p
          acceptable _                   = True
      in liftIO $ logMCond name severity msg acceptable

instance (HasLoggerName m) => HasLoggerName (SecureLogWrapped m) where
    getLoggerName = SecureLogWrapped getLoggerName
    modifyLoggerName foo (SecureLogWrapped m) =
        SecureLogWrapped (modifyLoggerName foo m)

execSecureLogWrapped :: SecureLogWrapped m a -> m a
execSecureLogWrapped (SecureLogWrapped act) = act

-- | Shortcut for 'logMessage' to use according severity.
logDebugS, logInfoS, logNoticeS, logWarningS, logErrorS
    :: (HasLoggerName m, MonadIO m)
    => Text -> m ()
logDebugS   = logMessageS Debug
logInfoS    = logMessageS Info
logNoticeS  = logMessageS Notice
logWarningS = logMessageS Warning
logErrorS   = logMessageS Error

-- | Same as 'logMesssage', but log to two loggers, put only insecure
-- version to memmode.
logMessageS
    :: (HasLoggerName m, MonadIO m)
    => Severity
    -> Text
    -> m ()
logMessageS severity t = execSecureLogWrapped $ do
    name <- getLoggerName
    dispatchMessage name severity t
