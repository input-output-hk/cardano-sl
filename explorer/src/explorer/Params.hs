-- | Getter params from Args

module Params
       ( gtSscParams
       , getLoggingParams
       , getNodeParams
       , getBaseParams
       , getPeersFromArgs
       ) where

import           Universum

import           Data.Default                     (def)
import           Mockable                         (Catch, Fork, Mockable, Throw)
import           System.Wlog                      (LoggerName, WithLogger)

import qualified Data.ByteString.Char8 as BS8 (unpack)
import qualified Network.Transport.TCP as TCP (TCPAddr (..), TCPAddrInfo (..))
import qualified Pos.Client.CLI                   as CLI
import           Pos.Core.Configuration           (HasConfiguration)
import           Pos.Crypto                       (VssKeyPair)
import           Pos.Launcher                     (BaseParams (..), LoggingParams (..),
                                                   NodeParams (..), TransportParams (..))
import           Pos.Network.CLI                  (intNetworkConfigOpts)
import           Pos.Network.Types                (NetworkConfig (..), Topology (..))
import           Pos.Ssc.GodTossing               (GtParams (..))
import           Pos.Ssc.GodTossing.Configuration (HasGtConfiguration)
import           Pos.Update.Params                (UpdateParams (..))
import           Pos.Util.TimeWarp                (NetworkAddress, readAddrFile)
import           Pos.Util.UserSecret              (peekUserSecret)

import           ExplorerOptions                  (Args (..))
import           Secrets                          (updateUserSecretVSS,
                                                   userSecretWithGenesisKey)



gtSscParams :: Args -> VssKeyPair -> GtParams
gtSscParams Args {..} vssSK =
    GtParams
    { gtpSscEnabled = True
    , gtpVssKeyPair = vssSK
    , gtpBehavior   = def
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
        TopologyBehindNAT{} -> TCP.Unaddressable
        _ -> let (bindHost, bindPort) = bindAddress args
                 (externalHost, externalPort) = externalAddress args
                 tcpHost = BS8.unpack bindHost
                 tcpPort = show bindPort
                 tcpMkExternal = const (BS8.unpack externalHost, show externalPort)
             in  TCP.Addressable $ TCP.TCPAddrInfo tcpHost tcpPort tcpMkExternal

getNodeParams
    :: ( MonadIO        m
       , MonadFail      m
       , MonadThrow     m
       , WithLogger     m
       , Mockable Fork  m
       , Mockable Catch m
       , Mockable Throw m
       , HasConfiguration
       , HasGtConfiguration
       )
    => Args -> m NodeParams
getNodeParams args@Args {..} = do
    (primarySK, userSecret) <-
        userSecretWithGenesisKey args =<<
        updateUserSecretVSS args =<<
        peekUserSecret keyfilePath

    npNetworkConfig <- intNetworkConfigOpts networkConfigOpts
    let npTransport = getTransportParams args npNetworkConfig

    return NodeParams
        { npDbPathM = dbPath
        , npRebuildDb = rebuildDB
        , npSecretKey = primarySK
        , npUserSecret = userSecret
        , npBaseParams = getBaseParams "node" args
        , npJLFile = jlPath
        , npUpdateParams = UpdateParams
            { upUpdatePath = "explorer-update"
            , upUpdateWithPkg = True
            , upUpdateServers = CLI.updateServers commonArgs
            }
        , npReportServers = CLI.reportServers commonArgs
        , npBehaviorConfig = def
        , npUseNTP = not noNTP
        , npEnableMetrics = enableMetrics
        , npEkgParams = ekgParams
        , npStatsdParams = statsdParams
        , ..
        }
