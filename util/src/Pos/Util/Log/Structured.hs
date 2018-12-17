module Pos.Util.Log.Structured
       ( logMessageX
       , logDebugX
       , logInfoX
       , logNoticeX
       , logWarningX
       , logErrorX
         -- * Safe logging
       , logMessageSX
       , logDebugSX
       , logInfoSX
       , logNoticeSX
       , logWarningSX
       , logErrorSX
       , logMessagePX
       , logDebugPX
       , logInfoPX
       , logNoticePX
       , logWarningPX
       , logErrorPX
       ) where

import           Universum

import           Pos.Util.Log (ToObject)
import           Pos.Util.Log.LogSafe (selectPublicLogs, selectSecretLogs)
import           Pos.Util.Wlog.Compatibility (HasLoggerName (..), Severity (..),
                     logMX, logXCond)

-- | Shortcut for 'logMessageX' to use according severity.
logDebugX, logInfoX, logNoticeX, logWarningX, logErrorX
    :: (HasLoggerName m, MonadIO m, ToObject a)
    => a -> m ()
logDebugX   = logMessageX Debug
logInfoX    = logMessageX Info
logNoticeX  = logMessageX Notice
logWarningX = logMessageX Warning
logErrorX   = logMessageX Error

-- | Log an item in JSON format (only for JSON scribes).
logMessageX
    :: (HasLoggerName m, MonadIO m, ToObject a)
    => Severity
    -> a
    -> m ()
logMessageX severity a = do
    name <- askLoggerName
    logMX name severity a

-- | Shortcut for 'logMessageSX' to use according severity.
logDebugSX, logInfoSX, logNoticeSX, logWarningSX, logErrorSX
    :: (HasLoggerName m, MonadIO m, ToObject a)
    => a -> m ()
logDebugSX   = logMessageSX Debug
logInfoSX    = logMessageSX Info
logNoticeSX  = logMessageSX Notice
logWarningSX = logMessageSX Warning
logErrorSX   = logMessageSX Error

-- | Log an item in JSON format (only for secret JSON scribes).
logMessageSX
    :: (HasLoggerName m, MonadIO m, ToObject a)
    => Severity
    -> a
    -> m ()
logMessageSX severity a = do
    name <- askLoggerName
    logXCond name severity a selectSecretLogs

-- | Shortcut for 'logMessagePX' to use according severity.
logDebugPX, logInfoPX, logNoticePX, logWarningPX, logErrorPX
    :: (HasLoggerName m, MonadIO m, ToObject a)
    => a -> m ()
logDebugPX   = logMessagePX Debug
logInfoPX    = logMessagePX Info
logNoticePX  = logMessagePX Notice
logWarningPX = logMessagePX Warning
logErrorPX   = logMessagePX Error

-- | Log an item in JSON format (only for public JSON scribes).
logMessagePX
    :: (HasLoggerName m, MonadIO m, ToObject a)
    => Severity
    -> a
    -> m ()
logMessagePX severity a = do
    name <- askLoggerName
    logXCond name severity a selectPublicLogs
