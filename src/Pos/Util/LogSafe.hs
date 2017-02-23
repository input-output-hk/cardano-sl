-- | Safe/secure logging

module Pos.Util.LogSafe
       ( SecureLogWrapped(..)
       , logDebugS
       , logInfoS
       , logNoticeS
       , logWarningS
       , logErrorS
       ) where

import           Control.Monad.Trans (MonadTrans)
import           Data.List           (isSuffixOf)
import qualified Data.Text           as T
import qualified System.Log.Logger   as L
import           System.Wlog         (CanLog (..), HasLoggerName (..), Severity (..),
                                      convertSeverity, loggerName)
import           Universum


newtype SecureLogWrapped m a = SecureLogWrapped
    { getSecureLogWrapped :: m a
    } deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans SecureLogWrapped where
    lift = SecureLogWrapped

instance (MonadIO m) => CanLog (SecureLogWrapped m) where
    dispatchMessage
        (loggerName      -> name)
        (convertSeverity -> prior)
        msg =
      let acceptable p
              | "fileHandler" `isPrefixOf` p ||
                "rollerHandler" `isPrefixOf` p
              = not $ ".pub" `isSuffixOf` p
              | otherwise = True
      in liftIO $ L.logMCond name prior (T.unpack msg) acceptable

instance (Monad m, HasLoggerName m) => HasLoggerName (SecureLogWrapped m) where
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
