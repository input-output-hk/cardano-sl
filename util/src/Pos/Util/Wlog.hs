{-# LANGUAGE Rank2Types #-}

-- | an interface to 'log-warper'
--  functions and types gradually migrate towards 'katip'

module Pos.Util.Wlog
        ( -- * CanLog
          CanLog (..)
        , WithLogger
          -- * Pure logging
        , dispatchEvents        -- call sites: 1 chain/src/Pos/Chain/Ssc/Toss/Pure.hs
        , LogEvent (..)         -- call sites: 3 chain/src/Pos/Chain/Ssc/Toss/Pure.hs,db/src/Pos/DB/Update/Poll/Pure.hs,wallet-new/test/unit/UTxO/Verify.hs
        , NamedPureLogger (..)
        , launchNamedPureLog    -- call sites: 8   chain,db,explorer
        , runNamedPureLog       -- call sites: 2   chain,db
          -- * Setup
        , setupLogging          -- call sites: 7 generator,lib,networking(bench),tools:keygen|launcher
          -- * Logging functions
        , logDebug
        , logError
        , logInfo
        , logNotice
        , logWarning
        , logMessage            -- call sites: 3  infra,wallet-new
          -- * LoggerName
        , LoggerName
        , LoggerNameBox (..)    -- call sites: 9  core,db,infra,lib,tools,util
        , HasLoggerName (..)
        , usingLoggerName       -- call sites: 22 db,explorer,infra,lib,networking,node-ipc,tools,wallet,wallet-new
          -- * LoggerConfig
        , LoggerConfig (..)     -- call sites: 13  (mostly together with 'setupLogging')
        , lcTermSeverityOut     -- call sites: 1 tools/src/launcher/Main.hs
        , lcTree                -- call sites: 4 infra,lib,networking,tools
        , parseLoggerConfig     -- call sites: 2 lib,networking
          -- * Hierarchical tree of loggers (with lenses)
        , HandlerWrap (..)      -- call sites: 1 tools/src/launcher/Main.hs
        -- , hwFilePath            -- call sites: 1 infra/src/Pos/Infra/Reporting/Wlog.hs
        -- , ltFiles               -- call sites: 2 infra/.../Reporting/Wlog.hs,tools/src/launcher/Main.hs
        , ltSeverity            -- call sites: 5 networking/src/Bench/Network/Commons.hs,tools/src/launcher/Main.hs
          -- * Builders for 'LoggerConfig'
        , productionB           -- call sites: 6 lib,networking,tools
          -- * Severity
        , Severity (..)
        , debugPlus             -- call sites: 4 generator/app/VerificationBench.hs,tools:keygen|launcher
        , noticePlus            -- call sites: 1 networking/src/Network/Broadcast/OutboundQueue/Demo.hs
          -- * Saving Changes
        , retrieveLogContent    -- call sites: 1 infra/src/Pos/Infra/Reporting/Wlog.hs
        , updateGlobalLogger    -- call sites: 1 networking/src/Network/Broadcast/OutboundQueue/Demo.hs
          -- * Logging messages with a condition
        , logMCond              -- call sites: 1 core/src/Pos/Core/Util/LogSafe.hs
        --   -- * LogHandler
        -- , LogHandlerTag (HandlerFilelike)  -- call sites: 1 core/src/Pos/Core/Util/LogSafe.hs
        -- changed definition of secure/public log files
          -- * Utility functions
        , removeAllHandlers     -- call sites: 2 lib/src/Pos/Launcher/Resource.hs,networking/test/Test/Network/Broadcast/OutboundQueueSpec.hs
        , centiUtcTimeF         -- call sites: 1 networking/bench/LogReader/Main.hs
        , setLevel              -- call sites: 1 networking/src/Network/Broadcast/OutboundQueue/Demo.hs
        , setLogPrefix
        ) where

import           System.Wlog (HandlerWrap (..), debugPlus, lcTermSeverityOut,
                     ltSeverity, noticePlus, removeAllHandlers, setLevel,
                     updateGlobalLogger)
import           System.Wlog.Formatter (centiUtcTimeF)

import           Pos.Util.Wlog.Compatibility (CanLog (..), HasLoggerName (..),
                     LogEvent (..), LoggerConfig (..), LoggerName,
                     LoggerNameBox (..), NamedPureLogger (..), Severity (..),
                     WithLogger, dispatchEvents, launchNamedPureLog, lcTree,
                     logDebug, logError, logInfo, logMCond, logMessage,
                     logNotice, logWarning, parseLoggerConfig, productionB,
                     retrieveLogContent, runNamedPureLog, setLogPrefix,
                     setupLogging, usingLoggerName)

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
