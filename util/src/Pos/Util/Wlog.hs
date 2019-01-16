{-# LANGUAGE Rank2Types #-}

-- | a compatible interface to 'log-warper'
--   logging output is directed to 'katip'

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
        --   -- * Setup
        , setupLogging
        , setupLogging'
        -- , setupTestLogging
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
        -- , addLoggerName
        -- , setLoggerName
          -- * LoggerConfig
        , LoggerConfig
        -- , lcTree
        , parseLoggerConfig
          -- * Builders for 'LoggerConfig'
        , productionB
          -- * Severity
        , Severity (..)
          -- * Saving Changes
        , retrieveLogContent
        --   -- * Logging messages with a condition
        -- , logMCond
        --   -- * Utility functions
        , removeAllHandlers
        -- , centiUtcTimeF
        , setLogPrefix
        -- , getLinesLogged
        , defaultTestConfiguration
        , defaultInteractiveConfiguration
        , LoggingHandler (..)
        , LoggingHandlerInternal (..)
        ) where

import           Pos.Util.Klog.Compatibility (CanLog (..), HasLoggerName (..),
                     LogEvent (..), LoggerConfig, LoggerNameBox (..),
                     LoggingHandler (..), LoggingHandlerInternal (..),
                     NamedPureLogger (..), Severity (..), WithLogger,
                     defaultInteractiveConfiguration, defaultTestConfiguration,
                     dispatchEvents, launchNamedPureLog, logDebug, logError,
                     logInfo, logMessage, logNotice, logWarning,
                     parseLoggerConfig, productionB, removeAllHandlers,
                     retrieveLogContent, runNamedPureLog, setLogPrefix,
                     setupLogging, setupLogging', usingLoggerName)
import           Pos.Util.Log (LoggerName)
