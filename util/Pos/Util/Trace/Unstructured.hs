-- | Unstructured logging via Pos.Util.Trace: a text message with severity
-- and privacy levels.

{-# LANGUAGE GADTSyntax #-}

module Pos.Util.Trace.Unstructured
    ( Severity (..)
    , LogItem (..)
    , LogPrivacy (..)

    , publicLogItem
    , privateLogItem
    , publicPrivateLogItem

    , traceLogItem

    , logDebug
    , logError
    , logInfo
    , logNotice
    , logWarning

    , logDebugP
    , logErrorP
    , logInfoP
    , logNoticeP
    , logWarningP

    , logDebugS
    , logErrorS
    , logInfoS
    , logNoticeS
    , logWarningS

    , LogSecurityLevel (..)
    , traceLogItemSP
    , logDebugSP
    , logErrorSP
    , logInfoSP
    , logNoticeSP
    , logWarningSP

    , logException
    , bracketWithLogging

    ) where

import           Universum
import qualified Control.Exception as E

import           Pos.Util.Trace (Trace (..), traceWith)

data Severity where
    Debug   :: Severity
    Info    :: Severity
    Notice  :: Severity
    Warning :: Severity
    Error   :: Severity

data LogPrivacy where
    -- | Only to public logs.
    Public  :: LogPrivacy
    -- | Only to private logs.
    Private :: LogPrivacy
    -- | To public and private logs.
    Both    :: LogPrivacy

-- | An unstructured log item.
data LogItem = LogItem
    { liPrivacy    :: LogPrivacy
    , liSeverity   :: Severity
    , liMessage    :: Text
    }

publicLogItem :: (Severity, Text) -> LogItem
publicLogItem = uncurry (LogItem Public)

privateLogItem :: (Severity, Text) -> LogItem
privateLogItem = uncurry (LogItem Private)

publicPrivateLogItem :: (Severity, Text) -> LogItem
publicPrivateLogItem = uncurry (LogItem Both)

traceLogItem
    :: Trace m LogItem
    -> LogPrivacy
    -> Severity
    -> Text
    -> m ()
traceLogItem logTrace privacy severity message =
    traceWith logTrace logItem
  where
    logItem = LogItem
        { liPrivacy = privacy
        , liSeverity = severity
        , liMessage = message
        }

logDebug, logInfo, logNotice, logWarning, logError
    :: Trace m LogItem -> Text -> m ()
logDebug logTrace   = traceLogItem logTrace Both Debug
logInfo logTrace    = traceLogItem logTrace Both Info
logNotice logTrace  = traceLogItem logTrace Both Notice
logWarning logTrace = traceLogItem logTrace Both Warning
logError logTrace   = traceLogItem logTrace Both Error

logDebugP, logInfoP, logNoticeP, logWarningP, logErrorP
    :: Trace m LogItem -> Text -> m ()
logDebugP logTrace   = traceLogItem logTrace Public Debug
logInfoP logTrace    = traceLogItem logTrace Public Info
logNoticeP logTrace  = traceLogItem logTrace Public Notice
logWarningP logTrace = traceLogItem logTrace Public Warning
logErrorP logTrace   = traceLogItem logTrace Public Error

logDebugS, logInfoS, logNoticeS, logWarningS, logErrorS
    :: Trace m LogItem -> Text -> m ()
logDebugS logTrace   = traceLogItem logTrace Private Debug
logInfoS logTrace    = traceLogItem logTrace Private Info
logNoticeS logTrace  = traceLogItem logTrace Private Notice
logWarningS logTrace = traceLogItem logTrace Private Warning
logErrorS logTrace   = traceLogItem logTrace Private Error

type SecuredText = LogSecurityLevel -> Text

data LogSecurityLevel where
    SecretLogLevel :: LogSecurityLevel
    PublicLogLevel :: LogSecurityLevel

-- | Log to public logs, and to private logs securely (the 'SecuredText' is
-- run at the 'SecretLogLevel').
traceLogItemSP
    :: Applicative m
    => Trace m LogItem
    -> Severity
    -> SecuredText
    -> m ()
traceLogItemSP logTrace severity securedText =
       traceLogItem logTrace Private severity (securedText SecretLogLevel)
    *> traceLogItem logTrace Public severity  (securedText PublicLogLevel)

logDebugSP, logInfoSP, logNoticeSP, logWarningSP, logErrorSP
    :: Applicative m => Trace m LogItem -> SecuredText -> m ()
logDebugSP logTrace   = traceLogItemSP logTrace Debug
logInfoSP logTrace    = traceLogItemSP logTrace Info
logNoticeSP logTrace  = traceLogItemSP logTrace Notice
logWarningSP logTrace = traceLogItemSP logTrace Warning
logErrorSP logTrace   = traceLogItemSP logTrace Error

-- | Log an exception if it's raised.
-- FIXME should not define here.
logException :: Trace IO Text -> IO a -> IO a
logException logTrace = E.handle (\e -> handler e >> E.throwIO e)
  where
    handler :: E.SomeException -> IO ()
    handler exc = traceWith logTrace ("logException: " <> show exc)

-- | 'bracket' which logs given message after acquiring the resource
-- and before releasing it.
-- FIXME should not define here.
bracketWithLogging
    :: Trace IO Text
    -> Text
    -> IO a
    -> (a -> IO b)
    -> (a -> IO c)
    -> IO c
bracketWithLogging logTrace msg acquire release = E.bracket acquire' release'
  where
    -- Logging goes into the 'acquire' and 'release', not into the continuation
    -- itself.
    acquire'   = acquire <* traceWith logTrace ("<bracketWithLogging:before> " <> msg)
    release' r = traceWith logTrace ("<bracketWithLogging:after> " <> msg) *> release r
