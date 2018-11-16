-- | Definition of a 'Reporter IO' which uses log-warper to gather logs and
-- uses the HTTP backend to send them to some server(s).

module Pos.Reporting.Production
    ( ProductionReporterParams (..)
    , productionReporter
    ) where

import           Universum

import           Control.Exception.Safe (catchIO)

import           Pos.Crypto (ProtocolMagic)
import           Pos.Infra.Diffusion.Types (Diffusion)
import           Pos.Infra.Reporting (Reporter (..))
import           Pos.Infra.Reporting.Http (reportNode)
import           Pos.Infra.Reporting.NodeInfo (extendWithNodeInfo)
import           Pos.Util.CompileInfo (CompileTimeInfo)
import           Pos.Util.Trace (Severity (Error), Trace, traceWith)
import           Pos.Util.Wlog (LoggerConfig)

data ProductionReporterParams = ProductionReporterParams
    { prpServers         :: ![Text]
    , prpLoggerConfig    :: !LoggerConfig
    , prpProtocolMagic   :: !ProtocolMagic
    , prpCompileTimeInfo :: !CompileTimeInfo
    , prpTrace           :: !(Trace IO (Severity, Text))
    }

productionReporter
    :: ProductionReporterParams
    -> Diffusion IO -- ^ Used to get status info, not to do any network stuff.
    -> Reporter IO
productionReporter params diffusion = Reporter $ \rt -> do
    rt' <- extendWithNodeInfo diffusion rt
    reportNode logTrace protocolMagic compileTimeInfo servers rt'
        `catchIO`
        reportExnHandler rt'
  where
    servers = prpServers params
    protocolMagic = prpProtocolMagic params
    compileTimeInfo = prpCompileTimeInfo params
    logTrace = prpTrace params
    --
    reportExnHandler rt e =
        let msgToLog = "reportNode encountered IOException `" <> show e
                    <> "` while trying to report the message:" <> show rt
         in liftIO (traceWith logTrace (Error, msgToLog))
