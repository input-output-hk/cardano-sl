-- | Unstructured logging via Pos.Util.Trace: a text message with severity
-- and privacy levels.

module Pos.Util.Trace.Unstructured
    ( LogItem (..)
    , LogPrivacy (..)

    , publicLogItem
    , privateLogItem
    , publicPrivateLogItem

    , setupLogging

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
    ) where

import           Universum

import           Data.Functor.Contravariant (Op (..))
import qualified Pos.Util.Log as Log
import           Pos.Util.Trace (Trace (..), traceWith)


data LogPrivacy =
      Public       -- only to public logs.
    | PublicUnsafe -- only to public logs, not console.
    | Private      -- only to private logs.
    | Both         -- to public and private logs.
    deriving (Show)

-- | An unstructured log item.
data LogItem = LogItem
    { liPrivacy  :: LogPrivacy
    , liSeverity :: Log.Severity
    , liMessage  :: Text
    } deriving (Show)

publicLogItem :: (Log.Severity, Text) -> LogItem
publicLogItem = uncurry (LogItem Public)

privateLogItem :: (Log.Severity, Text) -> LogItem
privateLogItem = uncurry (LogItem Private)

publicPrivateLogItem :: (Log.Severity, Text) -> LogItem
publicPrivateLogItem = uncurry (LogItem Both)

traceLogItem
    :: Trace m LogItem
    -> LogPrivacy
    -> Log.Severity
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
logDebug logTrace   = traceLogItem logTrace Both Log.Debug
logInfo logTrace    = traceLogItem logTrace Both Log.Info
logNotice logTrace  = traceLogItem logTrace Both Log.Notice
logWarning logTrace = traceLogItem logTrace Both Log.Warning
logError logTrace   = traceLogItem logTrace Both Log.Error

logDebugP, logInfoP, logNoticeP, logWarningP, logErrorP
    :: Trace m LogItem -> Text -> m ()
logDebugP logTrace   = traceLogItem logTrace Public Log.Debug
logInfoP logTrace    = traceLogItem logTrace Public Log.Info
logNoticeP logTrace  = traceLogItem logTrace Public Log.Notice
logWarningP logTrace = traceLogItem logTrace Public Log.Warning
logErrorP logTrace   = traceLogItem logTrace Public Log.Error

logDebugS, logInfoS, logNoticeS, logWarningS, logErrorS
    :: Trace m LogItem -> Text -> m ()
logDebugS logTrace   = traceLogItem logTrace Private Log.Debug
logInfoS logTrace    = traceLogItem logTrace Private Log.Info
logNoticeS logTrace  = traceLogItem logTrace Private Log.Notice
logWarningS logTrace = traceLogItem logTrace Private Log.Warning
logErrorS logTrace   = traceLogItem logTrace Private Log.Error

type SecuredText = LogSecurityLevel -> Text

data LogSecurityLevel = SecretLogLevel | PublicLogLevel

-- | Log to public logs, and to private logs securely (the 'SecuredText' is
-- run at the 'SecretLogLevel').
traceLogItemSP
    :: Applicative m
    => Trace m LogItem
    -> Log.Severity
    -> SecuredText
    -> m ()
traceLogItemSP logTrace severity securedText =
       traceLogItem logTrace Private severity (securedText SecretLogLevel)
    *> traceLogItem logTrace Public severity  (securedText PublicLogLevel)

logDebugSP, logInfoSP, logNoticeSP, logWarningSP, logErrorSP
    :: Applicative m => Trace m LogItem -> SecuredText -> m ()
logDebugSP logTrace   = traceLogItemSP logTrace Log.Debug
logInfoSP logTrace    = traceLogItemSP logTrace Log.Info
logNoticeSP logTrace  = traceLogItemSP logTrace Log.Notice
logWarningSP logTrace = traceLogItemSP logTrace Log.Warning
logErrorSP logTrace   = traceLogItemSP logTrace Log.Error

-- | setup logging and return a Trace
setupLogging :: MonadIO m => Log.LoggerConfig -> Log.LoggerName -> IO (Trace m LogItem)
setupLogging lc ln = do
    lh <- Log.setupLogging lc
    return $ unstructuredTrace ln lh

unstructuredTrace :: MonadIO m => Log.LoggerName -> Log.LoggingHandler -> Trace m LogItem
unstructuredTrace ln lh = Trace $ Op $ \logitem ->
    let severity = liSeverity logitem
        message = liMessage logitem
    in
    liftIO $ Log.usingLoggerName lh ln $ Log.logMessage severity message
