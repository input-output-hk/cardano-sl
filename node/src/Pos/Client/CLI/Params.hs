-- | Getter params from Args

module Pos.Client.CLI.Params
       ( loggingParams
       , getBaseParams
       , getKeyfilePath
       , getNodeParams
       , getTransportParams
       , gtSscParams
       ) where

import           Universum

import qualified Data.ByteString.Char8      as BS8 (unpack)
import           Mockable                   (Catch, Fork, Mockable)
import qualified Network.Transport.TCP      as TCP (TCPAddr (..), TCPAddrInfo (..))
import           System.Wlog                (LoggerName, WithLogger)

import           Pos.Constants              (isDevelopment)
import           Pos.Core.Types             (Timestamp (..))
import           Pos.Crypto                 (VssKeyPair)
import           Pos.Genesis                (devGenesisContext, devStakesDistr,
                                             genesisContextProduction)
import           Pos.Launcher               (BaseParams (..), LoggingParams (..),
                                             NodeParams (..), TransportParams (..))
import           Pos.Network.CLI            (intNetworkConfigOpts)
import           Pos.Network.Types          (NetworkConfig (..), Topology (..))
import           Pos.Security               (SecurityParams (..))
import           Pos.Ssc.GodTossing         (GtParams (..))
import           Pos.Update.Params          (UpdateParams (..))
import           Pos.Util.UserSecret        (peekUserSecret)

import           Pos.Client.CLI.NodeOptions (CommonNodeArgs (..), NodeArgs (..),
                                             maliciousEmulationAttacks,
                                             maliciousEmulationTargets)
import           Pos.Client.CLI.Options     (CommonArgs (..))
import           Pos.Client.CLI.Secrets     (updateUserSecretVSS,
                                             userSecretWithGenesisKey)


loggingParams :: LoggerName -> CommonNodeArgs -> LoggingParams
loggingParams tag CommonNodeArgs{..} =
    LoggingParams
    { lpHandlerPrefix = logPrefix commonArgs
    , lpConfigPath    = logConfig commonArgs
    , lpRunnerTag     = tag
    }

getBaseParams :: LoggerName -> CommonNodeArgs -> BaseParams
getBaseParams loggingTag args@CommonNodeArgs {..} =
    BaseParams { bpLoggingParams = loggingParams loggingTag args }

gtSscParams :: CommonNodeArgs -> VssKeyPair -> GtParams
gtSscParams CommonNodeArgs {..} vssSK =
    GtParams
    { gtpSscEnabled = True
    , gtpVssKeyPair = vssSK
    }

getKeyfilePath :: CommonNodeArgs -> FilePath
getKeyfilePath CommonNodeArgs {..}
    | isDevelopment = case devSpendingGenesisI of
          Nothing -> keyfilePath
          Just i  -> "node-" ++ show i ++ "." ++ keyfilePath
    | otherwise = keyfilePath

getNodeParams ::
       (MonadIO m, WithLogger m, Mockable Fork m, Mockable Catch m)
    => CommonNodeArgs
    -> NodeArgs
    -> Timestamp
    -> m NodeParams
getNodeParams cArgs@CommonNodeArgs{..} NodeArgs{..} systemStart = do
    (primarySK, userSecret) <-
        userSecretWithGenesisKey cArgs =<<
            updateUserSecretVSS cArgs =<<
                peekUserSecret (getKeyfilePath cArgs)
    npNetworkConfig <- intNetworkConfigOpts networkConfigOpts
    let npTransport = getTransportParams cArgs npNetworkConfig
        devStakeDistr =
            devStakesDistr
                (flatDistr commonArgs)
                (richPoorDistr commonArgs)
                (expDistr commonArgs)
    let npGenesisCtx
            | isDevelopment = devGenesisContext devStakeDistr
            | otherwise = genesisContextProduction
    pure NodeParams
        { npDbPathM = dbPath
        , npRebuildDb = rebuildDB
        , npSecretKey = primarySK
        , npUserSecret = userSecret
        , npSystemStart = systemStart
        , npBaseParams = getBaseParams "node" cArgs
        , npJLFile = jlPath
        , npReportServers = reportServers commonArgs
        , npUpdateParams = UpdateParams
            { upUpdatePath    = updateLatestPath
            , upUpdateWithPkg = updateWithPackage
            , upUpdateServers = updateServers commonArgs
            }
        , npSecurityParams = SecurityParams
            { spAttackTypes   = maliciousEmulationAttacks
            , spAttackTargets = maliciousEmulationTargets
            }
        , npUseNTP = not noNTP
        , npEnableMetrics = enableMetrics
        , npEkgParams = ekgParams
        , npStatsdParams = statsdParams
        , ..
        }

getTransportParams :: CommonNodeArgs -> NetworkConfig kademlia -> TransportParams
getTransportParams args networkConfig = TransportParams { tpTcpAddr = tcpAddr }
  where
    tcpAddr = case ncTopology networkConfig of
        TopologyBehindNAT{} -> TCP.Unaddressable
        _ -> let (bindHost, bindPort) = bindAddress args
                 (externalHost, externalPort) = externalAddress args
                 tcpHost = BS8.unpack bindHost
                 tcpPort = show bindPort
                 tcpMkExternal = const (BS8.unpack externalHost, show externalPort)
             in  TCP.Addressable $ TCP.TCPAddrInfo tcpHost tcpPort tcpMkExternal
