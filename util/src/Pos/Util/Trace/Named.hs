-- | 'Trace' for named logging.

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pos.Util.Trace.Named
    ( TraceNamed
    , LogNamed (..)
    , TrU.LogItem
    , named
    , setupLogging
    , namedTrace
    , appendName
    , fromTypeclassNamedTraceWlog
    -- * rexports
    , natTrace
    -- * log functions
    , logMessage, logMessageS, logMessageP
    , logDebug,   logDebugS,   logDebugP,   logDebugSP,   logDebugUnsafeP
    , logError,   logErrorS,   logErrorP,   logErrorSP,   logErrorUnsafeP
    , logInfo,    logInfoS,    logInfoP,    logInfoSP,    logInfoUnsafeP
    , logNotice,  logNoticeS,  logNoticeP,  logNoticeSP,  logNoticeUnsafeP
    , logWarning, logWarningS, logWarningP, logWarningSP, logWarningUnsafeP
    ) where

import           Universum

import           Data.Functor.Contravariant (Op (..), contramap)
import qualified Data.Text as Text
import qualified Pos.Util.Log as Log
import           Pos.Util.Log.LoggerConfig (LogSecurityLevel (..))
import           Pos.Util.Log.LogSafe (SecuredText, logMCond, logMessageUnsafeP,
                     selectPublicLogs, selectSecretLogs)
import           Pos.Util.Trace (Trace (..), natTrace, traceWith)
import qualified Pos.Util.Trace.Unstructured as TrU (LogItem (..),
                     LogPrivacy (..))
import qualified Pos.Util.Wlog as Wlog

type TraceNamed m = Trace m (LogNamed TrU.LogItem)

-- | Attach a 'LoggerName' to something.
data LogNamed item = LogNamed
    { lnName :: [Log.LoggerName]
    , lnItem :: item
    } deriving (Show)

traceNamedItem
    :: TraceNamed m
    -> TrU.LogPrivacy
    -> Log.Severity
    -> Text
    -> m ()
traceNamedItem logTrace p s m =
    traceWith (named logTrace) TrU.LogItem{ TrU.liPrivacy  = p
                                          , TrU.liSeverity = s
                                          , TrU.liMessage  = m
                                          }

logMessage, logMessageS, logMessageP :: TraceNamed m -> Log.Severity -> Text -> m ()
logMessage logTrace  = traceNamedItem logTrace TrU.Both
logMessageS logTrace = traceNamedItem logTrace TrU.Private
logMessageP logTrace = traceNamedItem logTrace TrU.Public

logDebug, logInfo, logNotice, logWarning, logError
    :: TraceNamed m -> Text -> m ()
logDebug logTrace   = traceNamedItem logTrace TrU.Both Log.Debug
logInfo logTrace    = traceNamedItem logTrace TrU.Both Log.Info
logNotice logTrace  = traceNamedItem logTrace TrU.Both Log.Notice
logWarning logTrace = traceNamedItem logTrace TrU.Both Log.Warning
logError logTrace   = traceNamedItem logTrace TrU.Both Log.Error
logDebugS, logInfoS, logNoticeS, logWarningS, logErrorS
    :: TraceNamed m -> Text -> m ()
logDebugS logTrace   = traceNamedItem logTrace TrU.Private Log.Debug
logInfoS logTrace    = traceNamedItem logTrace TrU.Private Log.Info
logNoticeS logTrace  = traceNamedItem logTrace TrU.Private Log.Notice
logWarningS logTrace = traceNamedItem logTrace TrU.Private Log.Warning
logErrorS logTrace   = traceNamedItem logTrace TrU.Private Log.Error
logDebugP, logInfoP, logNoticeP, logWarningP, logErrorP
    :: TraceNamed m -> Text -> m ()
logDebugP logTrace   = traceNamedItem logTrace TrU.Public Log.Debug
logInfoP logTrace    = traceNamedItem logTrace TrU.Public Log.Info
logNoticeP logTrace  = traceNamedItem logTrace TrU.Public Log.Notice
logWarningP logTrace = traceNamedItem logTrace TrU.Public Log.Warning
logErrorP logTrace   = traceNamedItem logTrace TrU.Public Log.Error
logDebugSP, logInfoSP, logNoticeSP, logWarningSP, logErrorSP
    :: Monad m => TraceNamed m -> SecuredText -> m ()
logDebugSP logTrace   f = logDebugS logTrace (f SecretLogLevel) >> logDebugP logTrace (f PublicLogLevel)
logInfoSP logTrace    f = logInfoS logTrace (f SecretLogLevel) >> logInfoP logTrace (f PublicLogLevel)
logNoticeSP logTrace  f = logNoticeS logTrace (f SecretLogLevel) >> logNoticeP logTrace (f PublicLogLevel)
logWarningSP logTrace f = logWarningS logTrace (f SecretLogLevel) >> logWarningP logTrace (f PublicLogLevel)
logErrorSP logTrace   f = logErrorS logTrace (f SecretLogLevel) >> logErrorP logTrace (f PublicLogLevel)
logDebugUnsafeP, logInfoUnsafeP, logNoticeUnsafeP, logWarningUnsafeP, logErrorUnsafeP
    :: TraceNamed m -> Text -> m ()
logDebugUnsafeP logTrace   = traceNamedItem logTrace TrU.PublicUnsafe Log.Debug
logInfoUnsafeP logTrace    = traceNamedItem logTrace TrU.PublicUnsafe Log.Info
logNoticeUnsafeP logTrace  = traceNamedItem logTrace TrU.PublicUnsafe Log.Notice
logWarningUnsafeP logTrace = traceNamedItem logTrace TrU.PublicUnsafe Log.Warning
logErrorUnsafeP logTrace   = traceNamedItem logTrace TrU.PublicUnsafe Log.Error

modifyName
    :: ([Log.LoggerName] -> [Log.LoggerName])
    -> Trace m (LogNamed i)
    -> Trace m (LogNamed i)
modifyName k = contramap f
  where
    f (LogNamed name item) = LogNamed (k name) item

appendName :: Log.LoggerName -> Trace m (LogNamed i) -> Trace m (LogNamed i)
appendName lname = modifyName (\e -> [lname] <> e)

named :: Trace m (LogNamed i) -> Trace m i
named = contramap (LogNamed mempty)

-- | setup logging and return a Trace
setupLogging
    :: MonadIO m
    => Text -> Log.LoggerConfig -> Log.LoggerName -> m (TraceNamed m)
setupLogging cfoKey lc ln = do
    lh <- liftIO $ Log.setupLogging cfoKey lc
    let nt = namedTrace lh
    return $ appendName ln nt

-- | Use the magic CanLog IO instance from Wlog.Compatibility to conjure a
-- trace. Not recommended for further use.
fromTypeclassNamedTraceWlog
    :: ( Wlog.CanLog m )
    => Trace m (LogNamed (Wlog.Severity, Text))
fromTypeclassNamedTraceWlog = Trace $ Op $ \namedI ->
    let name       = Text.intercalate (Text.pack ".") (lnName namedI)
        (sev, txt) = lnItem namedI
    in  Wlog.dispatchMessage name sev txt

namedTrace
    :: MonadIO m => Log.LoggingHandler -> TraceNamed m
namedTrace lh = Trace $ Op $ \namedLogitem ->
    let loggerNames =  lnName namedLogitem
        litem = lnItem namedLogitem
        privacy  = TrU.liPrivacy litem
        severity = TrU.liSeverity litem
        message  = TrU.liMessage litem
    in
    liftIO $ case privacy of
        TrU.Both    -> Log.usingLoggerNames lh loggerNames $
            Log.logMessage severity message
        -- pass to every logging scribe
        TrU.Public  -> Log.usingLoggerNames lh loggerNames $
            logMCond lh severity message selectPublicLogs
        -- pass to logging scribes that are marked as
        -- public (LogSecurityLevel == PublicLogLevel).
        TrU.PublicUnsafe  -> Log.usingLoggerNames lh loggerNames $
            logMessageUnsafeP severity lh message
        -- pass to logging scribes that are marked as
        -- public (LogSecurityLevel == PublicLogLevel).
        TrU.Private -> Log.usingLoggerNames lh loggerNames $
            logMCond lh severity message selectSecretLogs
        -- pass to logging scribes that are marked as
        -- private (LogSecurityLevel == SecretLogLevel).

{- testing:

logTrace' <- setupLogging "test" (Pos.Util.Log.LoggerConfig.defaultInteractiveConfiguration Log.Debug) "named"
let li = publicLogItem (Log.Debug, "testing")
    ni = namedItem "Tests" li

traceWith logTrace' ni
traceWith (named $ appendName "more" logTrace') li


logTrace' <- setupLogging "test" (Pos.Util.Log.LoggerConfig.jsonInteractiveConfiguration Log.Debug) "named"
logDebug logTrace' "hello"
logDebug (appendName "blabla" logTrace') "hello"
-}
