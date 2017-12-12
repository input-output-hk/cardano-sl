{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE TypeFamilies              #-}

-- | Safe/secure logging

module Pos.Util.LogSafe
       ( -- * Logging functions
         SelectiveLogWrapped(..)
       , logDebugS
       , logInfoS
       , logNoticeS
       , logWarningS
       , logErrorS
       , logDebugUnsafeP
       , logInfoUnsafeP
       , logNoticeUnsafeP
       , logWarningUnsafeP
       , logErrorUnsafeP
       , logDebugSP
       , logInfoSP
       , logNoticeSP
       , logWarningSP
       , logErrorSP

         -- * Secure 'Buildable's
       , SecureLog (..)
       , LogSecurityLevel
       , secure
       , unsecure

         -- ** Secure formatters
       , secureF
       , plainOrSecureF
       , secretOnlyF
       , secureListF

       , buildSafe

         -- ** Secure log utils
       , BuildableSafe
       , SecuredText
       , buildUnsecure
       , getSecuredText
       ) where

import           Universum

import           Control.Monad.Trans (MonadTrans)
import           Data.List (isSuffixOf)
import           Data.Reflection (Reifies (..), reify)
import qualified Data.Text.Buildable
import           Data.Text.Lazy.Builder (Builder)
import           Formatting (bprint, build, fconst, later, mapf, (%))
import           Formatting.Internal (Format (..))
import           System.Wlog (CanLog (..), HasLoggerName (..), Severity (..), logMCond)
import           System.Wlog.LogHandler (LogHandlerTag (HandlerFilelike))

import           Pos.Core.Common (Coin)
import           Pos.Crypto (PassPhrase)


----------------------------------------------------------------------------
-- Logging
----------------------------------------------------------------------------

-- | Modifies logging so that it writes only to files specified by @s@ type.
newtype SelectiveLogWrapped s m a = SelectiveLogWrapped
    { getSecureLogWrapped :: m a
    } deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans (SelectiveLogWrapped s) where
    lift = SelectiveLogWrapped

-- | Whether to log to given log handler.
type SelectionMode = LogHandlerTag -> Bool

selectPublicLogs :: SelectionMode
selectPublicLogs = \case
    HandlerFilelike p -> ".pub" `isSuffixOf` p
    _ -> False

selectSecretLogs :: SelectionMode
selectSecretLogs = not . selectPublicLogs

instance (MonadIO m, Reifies s SelectionMode) =>
         CanLog (SelectiveLogWrapped s m) where
    dispatchMessage name severity msg =
        liftIO $ logMCond name severity msg (reflect (Proxy @s))

instance (HasLoggerName m) => HasLoggerName (SelectiveLogWrapped s m) where
    askLoggerName = SelectiveLogWrapped askLoggerName
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

-- | Same as 'logMesssage', but log to secret logs, put only insecure
-- version to memmode (to terminal).
logMessageS
    :: (HasLoggerName m, MonadIO m)
    => Severity
    -> Text
    -> m ()
logMessageS severity t =
    reify selectSecretLogs $ \s ->
    execSecureLogWrapped s $ do
        name <- askLoggerName
        dispatchMessage name severity t

----------------------------------------------------------------------------
-- Secure buildables
----------------------------------------------------------------------------

-- * Secutity log levels

-- Stuff below provides way to use different @instance Buildable@ for writing
-- to secret and public logs.
--
-- Example:
--
-- @
-- logInfo $ sformat ("Nyan: "%secureF build) Nyan
-- @
--
-- this function uses @instance Buildable (SecureLog Nyan)@ to display the item.
--
-- @
-- logInfoSP $ \sl -> sformat ("A diamond on the "%buildSafe sl) Rough
-- @
--
-- this function prints value to secret logs as is, while for public logs it uses
-- @instance Buildable (SecureLog Rough)@.
--
-- Logging to public logs only is marked as unsafe, because nothing prevents its
-- user from writting unsecure logs to public.
-- For this reason, if you don't use "security level parameter" on 'logInfoSP' call,
-- remain it as @logInfoSP $ \__sl -> ...@ in order to remember that secured text
-- should be provided inside.

-- Modifies 'instance Buildable' so that it doesn't contain sensitive info
newtype SecureLog a = SecureLog
    { getSecureLog :: a
    } deriving (Eq, Ord)

data LogSecurityLevel
    = SecretLogLevel
    | PublicLogLevel
    deriving (Eq)

secure :: LogSecurityLevel
secure = PublicLogLevel

unsecure :: LogSecurityLevel
unsecure = SecretLogLevel

buildUnsecure :: Buildable a => SecureLog a -> Builder
buildUnsecure (SecureLog a) = bprint build a

type BuildableSafe a = (Buildable a, Buildable (SecureLog a))

-- | Modifies single-parameter formatter to make it use public logging.
secureF :: Format r (SecureLog a -> r) -> Format r (a -> r)
secureF = mapf SecureLog

-- | Takes one of given items (usually formatters, nonsecure goes first),
-- depending on security level.
plainOrSecureF :: LogSecurityLevel -> a -> a -> a
plainOrSecureF SecretLogLevel fmt _ = fmt
plainOrSecureF PublicLogLevel _ fmt = fmt

buildSafe :: BuildableSafe a => LogSecurityLevel -> Format r (a -> r)
buildSafe sl = plainOrSecureF sl build (secureF build)

-- | Negates single-parameter formatter for public logs.
secretOnlyF :: LogSecurityLevel -> Format r (a -> r) -> Format r (a -> r)
secretOnlyF sl fmt = plainOrSecureF sl fmt (fconst "?")

-- | For public logs hides list content, showing only its size.
secureListF
    :: NontrivialContainer l
    => LogSecurityLevel -> Format r (l -> r) -> Format r (l -> r)
secureListF sl fmt = plainOrSecureF sl fmt lengthFmt
  where
    lengthFmt = later $ \l ->
        if null l
        then "[]"
        else bprint ("[... ("%build%" item(s))]") $ length l


-- | Same as 'logMesssage', put to public logs only (these logs don't go
-- to terminal). Use it along with 'logMessageS' when want to specify
-- secret and public log alternatives manually.
logMessageUnsafeP
    :: (HasLoggerName m, MonadIO m)
    => Severity
    -> Text
    -> m ()
logMessageUnsafeP severity t =
    reify selectPublicLogs $ \s ->
    execSecureLogWrapped s $ do
        name <- askLoggerName
        dispatchMessage name severity t

-- | Shortcut for 'logMessageUnsafeP' to use according severity.
logDebugUnsafeP, logInfoUnsafeP, logNoticeUnsafeP, logWarningUnsafeP, logErrorUnsafeP
    :: (HasLoggerName m, MonadIO m)
    => Text -> m ()
logDebugUnsafeP   = logMessageUnsafeP Debug
logInfoUnsafeP    = logMessageUnsafeP Info
logNoticeUnsafeP  = logMessageUnsafeP Notice
logWarningUnsafeP = logMessageUnsafeP Warning
logErrorUnsafeP   = logMessageUnsafeP Error


type SecuredText = LogSecurityLevel -> Text

getSecuredText :: LogSecurityLevel -> SecuredText -> Text
getSecuredText = (&)

-- | Same as 'logMesssageSP', put to public and secret logs securely.
logMessageSP
    :: (HasLoggerName m, MonadIO m)
    => Severity -> SecuredText -> m ()
logMessageSP severity securedText = do
    logMessageS severity $ securedText SecretLogLevel
    logMessageUnsafeP severity $ securedText PublicLogLevel

-- | Shortcut for 'logMessage' to use according severity.
logDebugSP, logInfoSP, logNoticeSP, logWarningSP, logErrorSP
    :: (HasLoggerName m, MonadIO m)
    => SecuredText -> m ()
logDebugSP   = logMessageSP Debug
logInfoSP    = logMessageSP Info
logNoticeSP  = logMessageSP Notice
logWarningSP = logMessageSP Warning
logErrorSP   = logMessageSP Error


instance Buildable (SecureLog PassPhrase) where
    build _ = "<passphrase>"

-- maybe I'm wrong here, but currently masking it important for wallet servant logs
instance Buildable (SecureLog Coin) where
    build _ = "? coin(s)"
