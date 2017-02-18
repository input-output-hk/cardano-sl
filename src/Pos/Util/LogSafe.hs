-- | Safe/secure logging

module Pos.Util.LogSafe
       ( SecureLogWrapped(..)
       , logDebugS
       , logInfoS
       , logNoticeS
       , logWarningS
       , logErrorS
       , HasSecureFormatting(..)
       ) where

import           Control.Monad.Trans (MonadTrans)
import           Formatting          (Format)
import           System.Wlog         (CanLog (..), HasLoggerName (..), Severity (..),
                                      WithLogger)
import           Universum


newtype SecureLogWrapped m a = SecureLogWrapped
    { getSecureLogWrapped :: m a
    } deriving (Functor, Applicative, Monad, CanLog)

instance MonadTrans SecureLogWrapped where
    lift = SecureLogWrapped

instance (Monad m, HasLoggerName m) => HasLoggerName (SecureLogWrapped m) where
    getLoggerName = (<> ".Secret") <$> SecureLogWrapped getLoggerName
    modifyLoggerName foo (SecureLogWrapped m) =
        SecureLogWrapped (modifyLoggerName foo m)

execSecureLogWrapped :: SecureLogWrapped m a -> m a
execSecureLogWrapped (SecureLogWrapped act) = act

class HasSecureFormatting a where
    secureF :: Bool -> Format r (a -> r)

-- | Shortcut for 'logMessage' to use according severity.
logDebugS, logInfoS, logNoticeS, logWarningS, logErrorS
    :: WithLogger m
    => Text -> m ()
logDebugS   = logMessageS Debug
logInfoS    = logMessageS Info
logNoticeS  = logMessageS Notice
logWarningS = logMessageS Warning
logErrorS   = logMessageS Error

-- | Same as 'logMesssage', but log to two loggers, put only insecure
-- version to memmode.
logMessageS
    :: WithLogger m
    => Severity
    -> Text
    -> m ()
logMessageS severity t = execSecureLogWrapped $ do
    name <- getLoggerName
    dispatchMessage name severity t
