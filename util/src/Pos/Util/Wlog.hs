{-# LANGUAGE Rank2Types #-}

-- | an interface to 'log-warper'
--  functions and types gradually migrate towards 'katip'

module Pos.Util.Wlog
        ( -- * CanLog
          CanLog (..)
        , WithLogger
          -- * Pure logging
        , dispatchEvents
        , LogEvent (..)
        , NamedPureLogger (..)
        , launchNamedPureLog
        , runNamedPureLog
          -- * Setup
        , setupLogging
          -- * Logging functions
        , logDebug
        , logError
        , logInfo
        , logNotice
        , logWarning
        , logMessage
          -- * LoggerName
        , LoggerName
        , LoggerNameBox (..)
        , HasLoggerName (..)
        , usingLoggerName
          -- * LoggerConfig
        , LoggerConfig (..)
        , lcTree
        , parseLoggerConfig
          -- * Builders for 'LoggerConfig'
        , productionB
          -- * Severity
        , Severity (..)
          -- * Saving Changes
        , retrieveLogContent
          -- * Logging messages with a condition
        , logMCond
          -- * Utility functions
        , removeAllHandlers
        , centiUtcTimeF
        , setLogPrefix
        , getLinesLogged
        ) where

import           Pos.Util.Log (LoggerName, Severity (..))
import           Pos.Util.Log.LoggerConfig (LoggerConfig (..), lcTree,
                     parseLoggerConfig, setLogPrefix)
import           Pos.Util.Wlog.Compatibility (CanLog (..), HasLoggerName (..),
                     LogEvent (..), LoggerNameBox (..), NamedPureLogger (..),
                     WithLogger, centiUtcTimeF, dispatchEvents, getLinesLogged,
                     launchNamedPureLog, logDebug, logError, logInfo, logMCond,
                     logMessage, logNotice, logWarning, productionB,
                     removeAllHandlers, retrieveLogContent, runNamedPureLog,
                     setupLogging, usingLoggerName)
