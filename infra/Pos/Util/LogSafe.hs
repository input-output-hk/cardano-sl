-- | Safe/secure logging

module Pos.Util.LogSafe
       ( SelectiveLogWrapped(..)
       , logDebugS
       , logInfoS
       , logNoticeS
       , logWarningS
       , logErrorS
       , logDebugP
       , logInfoP
       , logNoticeP
       , logWarningP
       , logErrorP

       , NonSensitive (..)
       , buildNonSensitiveUnsafe
       , buildNonSensitiveMaybe
       ) where

import           Universum

import           Control.Monad.Trans    (MonadTrans)
import           Data.List              (isSuffixOf)
import           Data.Reflection        (Reifies (..), reify)
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

newtype SelectiveLogWrapped s m a = SelectiveLogWrapped
    { getSecureLogWrapped :: m a
    } deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans (SelectiveLogWrapped s) where
    lift = SelectiveLogWrapped

type SelectionMode = LogHandlerTag -> Bool

selectPublicLogs :: SelectionMode
selectPublicLogs = \case
    HandlerFilelike p -> ".pub" `isSuffixOf` p
    _ -> False

selectSecretLogs :: SelectionMode
selectSecretLogs = not . selectPublicLogs

instance (MonadIO m, Reifies s SelectionMode) =>
         CanLog (SelectiveLogWrapped s m) where
    dispatchMessage (loggerName -> name) severity msg =
        liftIO $ logMCond name severity msg (reflect (Proxy @s))

instance (HasLoggerName m) => HasLoggerName (SelectiveLogWrapped s m) where
    getLoggerName = SelectiveLogWrapped getLoggerName
    modifyLoggerName foo (SelectiveLogWrapped m) =
        SelectiveLogWrapped (modifyLoggerName foo m)

execSecureLogWrapped :: Proxy s -> SelectiveLogWrapped s m a -> m a
execSecureLogWrapped _ (SelectiveLogWrapped act) = act

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
logMessageS severity t =
    reify selectSecretLogs $ \s ->
    execSecureLogWrapped s $ do
        name <- getLoggerName
        dispatchMessage name severity t

-- | Shortcut for 'logMessage' to use according severity.
logDebugP, logInfoP, logNoticeP, logWarningP, logErrorP
    :: (HasLoggerName m, MonadIO m)
    => Text -> m ()
logDebugP   = logMessageP Debug
logInfoP    = logMessageP Info
logNoticeP  = logMessageP Notice
logWarningP = logMessageP Warning
logErrorP   = logMessageP Error

-- | Same as 'logMesssage', but log to two loggers, put only secure
-- version to memmode.
logMessageP
    :: (HasLoggerName m, MonadIO m)
    => Severity
    -> Text
    -> m ()
logMessageP severity t =
    reify selectPublicLogs $ \s ->
    execSecureLogWrapped s $ do
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
