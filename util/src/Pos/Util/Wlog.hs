module Pos.Util.Wlog
        ( -- * CanLog
          CanLog (..)
        , WithLogger
        , dispatchEvents
          -- * Pure logging manipulation
        , LogEvent (..)
        , NamedPureLogger (..)
        , launchNamedPureLog
        , runNamedPureLog
          -- * Logging functions
        , logDebug
        , logError
        , logInfo
        , logNotice
        , logWarning
        , logMessage
          -- * LoggerName
        , LoggerName (..)
        , LoggerNameBox (..)
        , HasLoggerName (..)
        , usingLoggerName
          -- * LoggerConfig
        , LoggerConfig (..)
        , lcLogsDirectory
        , lcTermSeverityOut
        , lcTree
        , parseLoggerConfig
          -- * Hierarchical tree of loggers (with lenses)
        , HandlerWrap (..)
        , fromScratch
        , hwFilePath
        , ltFiles
        , ltSeverity
        , ltSubloggers
        , zoomLogger
          -- * Builders for 'LoggerConfig'
        , consoleActionB
        , maybeLogsDirB
        , showTidB
        , productionB
        , termSeveritiesOutB
          -- * Severity
        , Severity (..)
        , debugPlus
        , errorPlus
        , infoPlus
        , noticePlus
        , warningPlus
          -- * Saving Changes
        , retrieveLogContent
        , updateGlobalLogger
          -- * LogHandler
        , defaultHandleAction
          -- * Logging messages with a condition
        , logMCond
          -- * Utility functions
        , removeAllHandlers
          -- * Modifying Loggers
        , setLevel
          -- * Launcher
        , setupLogging
          -- * Formatter
        , centiUtcTimeF
          -- * LogHandler
        , LogHandlerTag (HandlerFilelike)
        ) where

import           System.Wlog (CanLog (..), HandlerWrap (..), HasLoggerName (..),
                     LogEvent (..), LoggerConfig (..), LoggerName (..),
                     LoggerNameBox (..), NamedPureLogger (..), Severity (..),
                     WithLogger, consoleActionB, debugPlus,
                     defaultHandleAction, dispatchEvents, errorPlus,
                     fromScratch, hwFilePath, infoPlus, launchNamedPureLog,
                     lcLogsDirectory, lcTermSeverityOut, lcTree, logDebug,
                     logError, logInfo, logMCond, logMessage, logNotice,
                     logWarning, ltFiles, ltSeverity, ltSubloggers,
                     maybeLogsDirB, modifyLoggerName, noticePlus,
                     parseLoggerConfig, productionB, removeAllHandlers,
                     retrieveLogContent, runNamedPureLog, setLevel,
                     setupLogging, showTidB, termSeveritiesOutB,
                     updateGlobalLogger, usingLoggerName, warningPlus,
                     zoomLogger)
import           System.Wlog.Formatter (centiUtcTimeF)
import           System.Wlog.LogHandler (LogHandlerTag (HandlerFilelike))
