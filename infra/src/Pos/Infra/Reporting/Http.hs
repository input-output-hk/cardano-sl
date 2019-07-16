{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pos.Infra.Reporting.Http
       ( sendReport
       , sendReportNodeImpl
       , reportNode
       ) where

import           Universum

import           Pos.Crypto (ProtocolMagic (..))
import           Pos.Util.CompileInfo (CompileTimeInfo)
import           Pos.Util.Trace (Severity (..), Trace)


-- | Given optional log file and report type, sends reports to URI
-- asked. The file, if given, must of course exist and be openable/readable
-- by this process. You probably want to use a temporary file.
-- Report server URI should be in form like
-- "http(s)://host:port/" without specified endpoint.
sendReport
    :: ProtocolMagic
    -> CompileTimeInfo
    -> Maybe FilePath   -- ^ Log file to read from
    -> reportType
    -> Text             -- ^ Application name
    -> String           -- ^ URI of the report server
    -> IO ()
sendReport _pm _compileInfo _mLogFile _reportType _appName _reportServerUri =
    pure ()

-- | Common code across node sending: tries to send logs to at least one
-- reporting server.
sendReportNodeImpl
    :: Trace IO (Severity, Text)
    -> ProtocolMagic
    -> CompileTimeInfo
    -> [Text]         -- ^ Report server URIs
    -> Maybe FilePath -- ^ Optional path to log file to send.
    -> reportType
    -> IO ()
sendReportNodeImpl _logTrace _protocolMagic _compileInfo _servers _mLogFile _reportType =
    pure ()

-- | Send a report to a given list of servers.
--
-- FIXME this does logging, but the 'Report' may include log files.
-- Should the logs from sending a report go into these log files?
--
-- Note that we are catching all synchronous exceptions, but don't
-- catch async ones ('catchAny' is from safe-exceptions)
-- If reporting is broken, we don't want it to affect anything else.
-- FIXME then perhaps all of this reporting-related IO should be done in an
-- isolated thread, rather than inline at the call site.
reportNode
    :: Trace IO (Severity, Text)
    -> ProtocolMagic
    -> CompileTimeInfo
    -> [Text]         -- ^ Servers
    -> Maybe FilePath -- ^ Logs.
    -> reportType
    -> IO ()
reportNode _logTrace _protocolMagic _compileInfo _reportServers _mLogs _reportType =
    pure ()
