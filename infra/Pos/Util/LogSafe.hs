-- | Safe/secure logging

module Pos.Util.LogSafe
       ( SecureLogWrapped(..)
       , logDebugS
       , logInfoS
       , logNoticeS
       , logWarningS
       , logErrorS

       , NonSensitive (..)
       , buildNonSensitiveUnsafe
       , buildNonSensitiveMaybe
       ) where

import           Universum

import           Control.Monad.Trans    (MonadTrans)
import           Data.List              (isSuffixOf)
import qualified Data.Text.Buildable
import           Data.Text.Lazy.Builder (Builder)
import           Formatting             (bprint, build)
import           System.Wlog            (CanLog (..), HasLoggerName (..), Severity (..),
                                         loggerName)
import           System.Wlog.Handler    (LogHandlerTag (HandlerFilelike))
import           System.Wlog.Logger     (logMCond)

import           Pos.Core.Types         (Coin)
import           Pos.Crypto             (PassPhrase)

----------------------------------------------------------------------------
-- Logging
----------------------------------------------------------------------------

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


----------------------------------------------------------------------------
-- Non sensitive buildables
----------------------------------------------------------------------------

-- | Makes any instance of printing typeclass (e.g. 'Buildable') produce
-- text without sensitive info.
newtype NonSensitive a = NonSensitive
    { getNonSensitive :: a
    } deriving (Eq, Ord)

instance Buildable (NonSensitive Text) where
    build (NonSensitive t) = bprint build t

-- | Useful when there's nothing to hide.
buildNonSensitiveUnsafe
    :: Buildable a
    => NonSensitive a -> Builder
buildNonSensitiveUnsafe (NonSensitive a) = bprint build a

-- | For some types saying whether they are specified or not may be unsafe.
-- But in cases when it isn't, you can use this function to define
-- 'instance Buildable (NonSensitive (Maybe t))'.
buildNonSensitiveMaybe
    :: Buildable (NonSensitive a)
    => NonSensitive (Maybe a) -> Builder
buildNonSensitiveMaybe (NonSensitive m) =
    maybe "" (bprint build . NonSensitive) m


instance Buildable (NonSensitive PassPhrase) where
    build = buildNonSensitiveUnsafe  -- passphrase is indeed hiden by default

-- maybe I'm wrong here, but currently masking it important for wallet servant logs
instance Buildable (NonSensitive Coin) where
    build _ = "? coin(s)"
