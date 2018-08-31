module Pos.Util.Wlog
        ( -- * CanLog
          CanLog (..)
        , WithLogger
        , dispatchEvents        -- call sites: 1 chain/src/Pos/Chain/Ssc/Toss/Pure.hs
          -- * Pure logging manipulation
        , LogEvent (..)
        , NamedPureLogger (..)
        , launchNamedPureLog    -- call sites: 8   chain,db,explorer
        , runNamedPureLog       -- call sites: 2   chain,db
          -- * Logging functions
        , logDebug
        , logError
        , logInfo
        , logNotice
        , logWarning
        , logMessage
          -- * LoggerName
        , LoggerName (..)
        , LoggerNameBox (..)    -- call sites: 9  core,db,infra,lib,tools,util
        , HasLoggerName (..)
        , usingLoggerName       -- call sites: 22 db,explorer,infra,lib,networking,node-ipc,tools,wallet,wallet-new
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



{-
  attempt for reducing complexity:

  1. count call sites per function
  2. comment out 0 usages
  3. group by "common usage pattern"
      * just logging functions
      * setup logging
      * logging configuration
      * pure logger?
      * only 'CanLog' imported
      * CanLog and WithLogger imported => redundant
      * CanLog and HasLoggerName imported => replace with 'WithLogger'
-}

