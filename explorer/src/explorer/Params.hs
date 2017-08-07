-- | Getter params from Args

module Params
       ( gtSscParams
       , getLoggingParams
       , getNodeParams
       , getBaseParams
       , getPeersFromArgs
       ) where

import           Universum

import           System.Wlog           (LoggerName, WithLogger)
import           Mockable              (Mockable, Fork)

import qualified Data.ByteString.Char8 as BS8 (unpack)
import qualified Network.Transport.TCP as TCP (TCPAddr (..), TCPAddrInfo (..))
import qualified Pos.CLI               as CLI
import           Pos.Constants         (isDevelopment)
import           Pos.Context           (mkGenesisTxpContext)
import           Pos.Core.Types        (Timestamp (..))
import           Pos.Crypto            (VssKeyPair)
import           Pos.Genesis           (devAddrDistr, devStakesDistr, genesisUtxo,
                                        genesisUtxoProduction)
import           Pos.Network.Types     (NetworkConfig (..), Topology (..))
import           Pos.Network.CLI       (intNetworkConfigOpts)
import           Pos.Launcher          (BaseParams (..), LoggingParams (..),
                                        TransportParams (..), NodeParams (..))
import           Pos.Security.Params   (SecurityParams (..))
import           Pos.Ssc.GodTossing    (GtParams (..))
import           Pos.Update.Params     (UpdateParams (..))
import           Pos.Util.TimeWarp     (NetworkAddress, readAddrFile)
import           Pos.Util.UserSecret   (peekUserSecret)


import           ExplorerOptions       (Args (..))
import           Secrets               (updateUserSecretVSS, userSecretWithGenesisKey)



gtSscParams :: Args -> VssKeyPair -> GtParams
gtSscParams Args {..} vssSK =
    GtParams
    { gtpSscEnabled = True
    , gtpVssKeyPair = vssSK
    }

getBaseParams :: LoggerName -> Args -> BaseParams
getBaseParams loggingTag args@Args {..} =
    BaseParams { bpLoggingParams = getLoggingParams loggingTag args }

getLoggingParams :: LoggerName -> Args -> LoggingParams
getLoggingParams tag Args{..} =
    LoggingParams
    { lpHandlerPrefix = CLI.logPrefix commonArgs
    , lpConfigPath    = CLI.logConfig commonArgs
    , lpRunnerTag = tag
    }

getPeersFromArgs :: Args -> IO [NetworkAddress]
getPeersFromArgs Args {..} = do
    filePeers <- maybe (pure []) readAddrFile dhtPeersFile
    pure $ dhtPeersList ++ filePeers

getTransportParams :: Args -> NetworkConfig kademlia -> TransportParams
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

getNodeParams
    :: (MonadIO m, MonadFail m, MonadThrow m, WithLogger m, Mockable Fork m)
    => Args -> Timestamp -> m NodeParams
getNodeParams args@Args {..} systemStart = do
    (primarySK, userSecret) <-
        userSecretWithGenesisKey args =<<
        updateUserSecretVSS args =<<
        peekUserSecret keyfilePath

    npNetworkConfig <- intNetworkConfigOpts networkConfigOpts
    let npTransport = getTransportParams args npNetworkConfig

    let devStakeDistr =
            devStakesDistr
                (CLI.flatDistr commonArgs)
                (CLI.bitcoinDistr commonArgs)
                (CLI.richPoorDistr commonArgs)
                (CLI.expDistr commonArgs)

    let npGenesisTxpCtx
            | isDevelopment = mkGenesisTxpContext $ genesisUtxo Nothing (devAddrDistr devStakeDistr)
            | otherwise =  mkGenesisTxpContext genesisUtxoProduction

    return NodeParams
        { npDbPathM = dbPath
        , npRebuildDb = rebuildDB
        , npSecretKey = primarySK
        , npUserSecret = userSecret
        , npSystemStart = systemStart
        , npBaseParams = getBaseParams "node" args
        , npJLFile = jlPath
        , npPropagation = not (CLI.disablePropagation commonArgs)
        , npUpdateParams = UpdateParams
            { upUpdatePath = "explorer-update"
            , upUpdateWithPkg = True
            , upUpdateServers = CLI.updateServers commonArgs
            }
        , npReportServers = CLI.reportServers commonArgs
        , npSecurityParams = SecurityParams
            { spAttackTypes   = []
            , spAttackTargets = []
            }
          , npUseNTP = not noNTP
          , npEnableMetrics = enableMetrics
          , npEkgParams = ekgParams
          , npStatsdParams = statsdParams
          , ..
        }
