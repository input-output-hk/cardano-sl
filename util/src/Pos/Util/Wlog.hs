-- | an interface to 'log-warper'

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
        , LoggerName (..)
        , LoggerNameBox (..)    -- call sites: 9  core,db,infra,lib,tools,util
        , HasLoggerName (..)
        , usingLoggerName       -- call sites: 22 db,explorer,infra,lib,networking,node-ipc,tools,wallet,wallet-new
          -- * LoggerConfig
        , LoggerConfig (..)     -- call sites: 13  (mostly together with 'setupLogging')
        , lcLogsDirectory       -- call sites: 1 tools/src/launcher/Main.hs
        , lcTermSeverityOut     -- call sites: 1 tools/src/launcher/Main.hs
        , lcTree                -- call sites: 4 infra,lib,networking,tools
        , parseLoggerConfig     -- call sites: 2 lib,networking
          -- * Hierarchical tree of loggers (with lenses)
        , HandlerWrap (..)      -- call sites: 1 tools/src/launcher/Main.hs
        , fromScratch           -- call sites: 1 networking/src/Bench/Network/Commons.hs
        , hwFilePath            -- call sites: 1 infra/src/Pos/Infra/Reporting/Wlog.hs
        , ltFiles               -- call sites: 2 infra/.../Reporting/Wlog.hs,tools/src/launcher/Main.hs
        , ltSeverity            -- call sites: 5 networking/src/Bench/Network/Commons.hs,tools/src/launcher/Main.hs
        , zoomLogger            -- call sites: 3 networking/src/Bench/Network/Commons.hs
        , ltSubloggers          -- call sites: 1 infra/src/Pos/Infra/Reporting/Wlog.hs
          -- * Builders for 'LoggerConfig'
        , consoleActionB        -- call sites: 3 generator/app/VerificationBench.hs,lib/src/Pos/Launcher/Resource.hs
        , maybeLogsDirB         -- call sites: 2 lib/src/Pos/Launcher/Resource.hs,networking/src/Bench/Network/Commons.hs
        , showTidB              -- call sites: 1 lib/src/Pos/Launcher/Resource.hs
        , productionB           -- call sites: 6 lib,networking,tools
        , termSeveritiesOutB    -- call sites: 2 generator/app/VerificationBench.hs,tools/src/keygen/Main.hs
          -- * Severity
        , Severity (..)
        , debugPlus             -- call sites: 4 generator/app/VerificationBench.hs,tools:keygen|launcher
        , errorPlus             -- call sites: 1 networking/src/Bench/Network/Commons.hs
        , infoPlus              -- call sites: 2 networking/src/Bench/Network/Commons.hs
        , noticePlus            -- call sites: 1 networking/src/Network/Broadcast/OutboundQueue/Demo.hs
        , warningPlus           -- call sites: 1 networking/src/Bench/Network/Commons.hs
          -- * Saving Changes
        , retrieveLogContent    -- call sites: 1 infra/src/Pos/Infra/Reporting/Wlog.hs
        , updateGlobalLogger    -- call sites: 1 networking/src/Network/Broadcast/OutboundQueue/Demo.hs
          -- * LogHandler
        , defaultHandleAction   -- call sites: 2 generator/app/VerificationBench.hs,lib/src/Pos/Launcher/Resource.hs
          -- * Logging messages with a condition
        , logMCond              -- call sites: 1 core/src/Pos/Core/Util/LogSafe.hs
          -- * LogHandler
        , LogHandlerTag (HandlerFilelike)  -- call sites: 1 core/src/Pos/Core/Util/LogSafe.hs
          -- * Utility functions
        , removeAllHandlers     -- call sites: 2 lib/src/Pos/Launcher/Resource.hs,networking/test/Test/Network/Broadcast/OutboundQueueSpec.hs
        , centiUtcTimeF         -- call sites: 1 networking/bench/LogReader/Main.hs
        , setLevel              -- call sites: 1 networking/src/Network/Broadcast/OutboundQueue/Demo.hs
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

