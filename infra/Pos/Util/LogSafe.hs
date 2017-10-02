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
       , logDebugP
       , logInfoP
       , logNoticeP
       , logWarningP
       , logErrorP
       , logDebugSP
       , logInfoSP
       , logNoticeSP
       , logWarningSP
       , logErrorSP

         -- * Secure 'Buildable's
       , SecureLog (..)
       , PublicLog
       , secureLog

         -- ** Secure formatters
       , secureF
       , secAndPubF
       , secretOnlyF
       , secretOnlyF2

         -- ** Secure log utils
       , BuildableSecure
       , SecuredText
       , SecuredTextBox (..)
       , buildUnsecure
       , getSecuredText
       , securedTextBox
       ) where

import           Universum

import           Control.Monad.Trans    (MonadTrans)
import           Data.Default           (Default (..))
import           Data.List              (isSuffixOf)
import           Data.Reflection        (Reifies (..), reify)
import qualified Data.Text.Buildable
import           Data.Text.Lazy.Builder (Builder)
import           Formatting             (bprint, build, fconst, mapf, (%))
import           Formatting.Internal    (Format (..))
import           System.Wlog            (CanLog (..), HasLoggerName (..), Severity (..),
                                         loggerName)
import           System.Wlog.Handler    (LogHandlerTag (HandlerFilelike))
import           System.Wlog.Logger     (logMCond)

import           Pos.Core.Types         (Coin)
import           Pos.Crypto             (PassPhrase)


----------------------------------------------------------------------------
-- Logging
----------------------------------------------------------------------------

-- | Modifies logging so that it writes only to files specified by @ s @ type.
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
        name <- getLoggerName
        dispatchMessage name severity t

----------------------------------------------------------------------------
-- Secure buildables
----------------------------------------------------------------------------

-- * Secutity log levels

-- Stuff below provides way to use different 'instance Buildable' for writing
-- to secret and public logs.
--
-- Example:
--
-- @
-- logInfoSP $ \sl -> ("Nyan: "%secureF sl build) Nyan
-- @
--
-- this function prints value to secret logs as is, while for public logs it uses
-- 'instance Buildable (PublicLog Nyan)'.
--
-- By analogy, 'logInfoP' will log to public logs only, requiring
-- 'instance Buildable (PublicLog ...)'. It also provides security level @sl@
-- parameter which is redundant, but allows to quickly switch
-- between 'logInfoSP' and 'logInfoP' and protects you from forgetting to use
-- secure 'Buildable'.

-- | This security level is used when we known a priori that the text is builded
-- for secret logs, and we can use exact `instance Buildable`.
data SecLogLevel

-- | Similar to 'SecLogLevel', requires special secure `instance Buildable`.
data PubLogLevel

-- | Needed to define "closed type instance" below.
-- It's used when exact logging security level it to be defined later,
-- and we have to "be ready" to provide ways to build text for both secret and
-- public logs.
data AltLogLevel s where
    AltSecLogLevel :: AltLogLevel SecLogLevel
    AltPubLogLevel :: AltLogLevel PubLogLevel

-- | Promotes 'AltLogLevel' constructors to type.
data ReflectedAltLogLevel rs


-- Modifies 'instance Buildable' to correspond given security level @ s @.
data SecureLog s a = SecureLog
    { getSecureLog :: a
    } deriving (Eq, Ord)

secureLog :: Proxy s -> a -> SecureLog s a
secureLog _ = SecureLog

coerce :: forall s2 s1 a. SecureLog s1 a -> SecureLog s2 a
coerce = SecureLog . getSecureLog

buildUnsecure :: Buildable a => (SecureLog __ a) -> Builder
buildUnsecure (SecureLog a) = bprint build a

type SecretLog = SecureLog SecLogLevel
type PublicLog = SecureLog PubLogLevel

-- | We can log as-is to secret logs.
instance Buildable a => Buildable (SecretLog a) where
    build = buildUnsecure

instance Buildable (PublicLog String) where
    build = buildUnsecure

instance Buildable (PublicLog Text) where
    build = buildUnsecure

type BuildableSecure a = (Buildable a, Buildable (PublicLog a))

instance ( BuildableSecure a
         , Reifies rs (AltLogLevel s)
         ) =>
         Buildable (SecureLog (ReflectedAltLogLevel rs) a) where
    build = case reflect (Proxy @rs) of
        AltSecLogLevel -> bprint build . coerce @SecLogLevel
        AltPubLogLevel -> bprint build . coerce @PubLogLevel

-- | Facilitates secure formatters usage.
instance IsString s => IsString (SecureLog __ s) where
    fromString = SecureLog . fromString

-- | Modifies single-parameter formatter to make it use secure logging.
secureF :: Proxy s -> Format r (SecureLog s a -> r) -> Format r (a -> r)
secureF _ = mapf SecureLog

class LogLevelUtils s where
    -- | Uses one of given formatters, first one for secret logs, second one for
    -- public logs.
    secAndPubF :: Proxy s -> Format a b -> Format a b -> Format a b

instance LogLevelUtils SecLogLevel where
    secAndPubF _ sec _ = sec

instance LogLevelUtils PubLogLevel where
    secAndPubF _ _ pub = pub

instance Reifies rs (AltLogLevel s) =>
         LogLevelUtils (ReflectedAltLogLevel rs) where
    secAndPubF _ =
        case reflect (Proxy @rs) of
            AltSecLogLevel -> secAndPubF (Proxy @SecLogLevel)
            AltPubLogLevel -> secAndPubF (Proxy @PubLogLevel)

-- | Negates single-parameter formatter for public logs.
secretOnlyF
    :: LogLevelUtils s
    => Proxy s -> Format r (a -> r) -> Format r (a -> r)
secretOnlyF p fmt = secAndPubF p fmt (fconst "")

-- | Negates 2-parameters formatter for public logs.
secretOnlyF2
    :: LogLevelUtils s
    => Proxy s -> Format r (a -> b -> r) -> Format r (a -> b -> r)
secretOnlyF2 p fmt = secAndPubF p fmt (fconst ""%fconst "")


-- | Same as 'logMesssage', put to public logs only (these logs don't go
-- to terminal). Use it along with 'logMessageS' when want to specify
-- secret and public log alternatives manually.
logMessageP
    :: (HasLoggerName m, MonadIO m)
    => Severity
    -> (Proxy PubLogLevel -> Text)
    -> m ()
logMessageP severity t =
    reify selectPublicLogs $ \s ->
    execSecureLogWrapped s $ do
        name <- getLoggerName
        dispatchMessage name severity (t Proxy)

-- | Shortcut for 'logMessageP' to use according severity.
logDebugP, logInfoP, logNoticeP, logWarningP, logErrorP
    :: (HasLoggerName m, MonadIO m)
    => (Proxy PubLogLevel -> Text) -> m ()
logDebugP   = logMessageP Debug
logInfoP    = logMessageP Info
logNoticeP  = logMessageP Notice
logWarningP = logMessageP Warning
logErrorP   = logMessageP Error

-- | Text for either secret or public logs
type SecuredText =
    forall rs s.
    Reifies rs (AltLogLevel s) =>
    Proxy (ReflectedAltLogLevel rs) -> Text

getSecuredText :: AltLogLevel s -> SecuredText -> Text
getSecuredText sl securedText =
    reify sl $ \(Proxy :: Proxy sl) ->
        securedText (Proxy @(ReflectedAltLogLevel sl))

-- | Same as 'logMesssageSP', put to public and secret logs securely.
logMessageSP
    :: (HasLoggerName m, MonadIO m)
    => Severity -> SecuredText -> m ()
logMessageSP severity t = do
    logMessageS severity $ getSecuredText AltSecLogLevel t
    logMessageP severity $ \_ -> getSecuredText AltPubLogLevel t

-- | Shortcut for 'logMessage' to use according severity.
logDebugSP, logInfoSP, logNoticeSP, logWarningSP, logErrorSP
    :: (HasLoggerName m, MonadIO m)
    => (forall rs s. Reifies rs (AltLogLevel s) =>
                       Proxy (ReflectedAltLogLevel rs) -> Text)
    -> m ()
logDebugSP   = logMessageSP Debug
logInfoSP    = logMessageSP Info
logNoticeSP  = logMessageSP Notice
logWarningSP = logMessageSP Warning
logErrorSP   = logMessageSP Error


-- | Wrapper over quantificated 'SecuredText'.
data SecuredTextBox = SecuredTextBox SecuredText

securedTextBox :: BuildableSecure a => a -> SecuredTextBox
securedTextBox a = SecuredTextBox $ \sl -> pretty $ secureLog sl a

instance Default SecuredTextBox where
    def = securedTextBox @Text ""


instance Buildable (PublicLog PassPhrase) where
    build _ = "<passphrase>"

-- maybe I'm wrong here, but currently masking it important for wallet servant logs
instance Buildable (PublicLog Coin) where
    build _ = "? coin(s)"
