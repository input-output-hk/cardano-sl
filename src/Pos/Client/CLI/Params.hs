-- | Getter params from Args

module Pos.Client.CLI.Params
       ( loggingParams
       , getSimpleNodeParams
       , gtSscParams
       ) where

import           Universum

import qualified Data.ByteString.Char8 as BS8 (unpack)
import           Mockable              (Fork, Mockable)
import qualified Network.Transport.TCP as TCP (TCPAddr (..), TCPAddrInfo (..))
import           System.Wlog           (LoggerName, WithLogger)

import qualified Pos.CLI               as CLI
import           Pos.Constants         (isDevelopment)
import           Pos.Core.Types        (Timestamp (..))
import           Pos.Crypto            (VssKeyPair)
import           Pos.Genesis           (GenesisContext (..), devAddrDistr, devStakesDistr,
                                        genesisContextProduction, genesisUtxo)
import           Pos.Launcher          (BaseParams (..), LoggingParams (..),
                                        NodeParams (..), TransportParams (..))
import           Pos.Network.CLI       (intNetworkConfigOpts)
import           Pos.Network.Types     (NetworkConfig (..), Topology (..))
import           Pos.Security          (SecurityParams (..))
import           Pos.Ssc.GodTossing    (GtParams (..))
import           Pos.Update.Params     (UpdateParams (..))
import           Pos.Util.UserSecret   (peekUserSecret)

import           Pos.Client.CLI.NodeOptions           (SimpleNodeArgs (..))
import           Pos.Client.CLI.Secrets               (updateUserSecretVSS, userSecretWithGenesisKey)


loggingParams :: LoggerName -> SimpleNodeArgs -> LoggingParams
loggingParams tag SimpleNodeArgs{..} =
    LoggingParams
    { lpHandlerPrefix = CLI.logPrefix commonArgs
    , lpConfigPath    = CLI.logConfig commonArgs
    , lpRunnerTag     = tag
    }

getBaseParams :: LoggerName -> SimpleNodeArgs -> BaseParams
getBaseParams loggingTag args@SimpleNodeArgs {..} =
    BaseParams { bpLoggingParams = loggingParams loggingTag args }

gtSscParams :: SimpleNodeArgs -> VssKeyPair -> GtParams
gtSscParams SimpleNodeArgs {..} vssSK =
    GtParams
    { gtpSscEnabled = True
    , gtpVssKeyPair = vssSK
    }

getKeyfilePath :: SimpleNodeArgs -> FilePath
getKeyfilePath SimpleNodeArgs {..}
    | isDevelopment = case devSpendingGenesisI of
          Nothing -> keyfilePath
          Just i  -> "node-" ++ show i ++ "." ++ keyfilePath
    | otherwise = keyfilePath

getSimpleNodeParams ::
       (MonadIO m, MonadFail m, MonadThrow m, WithLogger m, Mockable Fork m)
    => SimpleNodeArgs
    -> Timestamp
    -> m NodeParams
getSimpleNodeParams args@SimpleNodeArgs {..} systemStart = do
    (primarySK, userSecret) <-
        userSecretWithGenesisKey args =<<
            updateUserSecretVSS args =<<
                peekUserSecret (getKeyfilePath args)
    npNetworkConfig <- intNetworkConfigOpts networkConfigOpts
    let npTransport = getTransportParams args npNetworkConfig
        devStakeDistr =
            devStakesDistr
                (CLI.flatDistr commonArgs)
                (CLI.bitcoinDistr commonArgs)
                (CLI.richPoorDistr commonArgs)
                (CLI.expDistr commonArgs)
    let npGenesisCtx
            | isDevelopment =
              let (aDistr,bootStakeholders) = devAddrDistr devStakeDistr
              in GenesisContext (genesisUtxo bootStakeholders aDistr)
                                bootStakeholders
            | otherwise = genesisContextProduction
    pure NodeParams
        { npDbPathM = dbPath
        , npRebuildDb = rebuildDB
        , npSecretKey = primarySK
        , npUserSecret = userSecret
        , npSystemStart = systemStart
        , npBaseParams = getBaseParams "node" args
        , npJLFile = jlPath
        , npReportServers = CLI.reportServers commonArgs
        , npUpdateParams = UpdateParams
            { upUpdatePath    = updateLatestPath
            , upUpdateWithPkg = updateWithPackage
            , upUpdateServers = CLI.updateServers commonArgs
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

getTransportParams :: SimpleNodeArgs -> NetworkConfig kademlia -> TransportParams
getTransportParams args networkConfig = TransportParams { tpTcpAddr = tcpAddr }
  where
    tcpAddr = case ncTopology networkConfig of
        TopologyBehindNAT _ -> TCP.Unaddressable
        _ -> let (bindHost, bindPort) = bindAddress args
                 (externalHost, externalPort) = externalAddress args
                 tcpHost = BS8.unpack bindHost
                 tcpPort = show bindPort
                 tcpMkExternal = const (BS8.unpack externalHost, show externalPort)
             in  TCP.Addressable $ TCP.TCPAddrInfo tcpHost tcpPort tcpMkExternal
