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
import           Data.Default               (def)
import qualified Data.Yaml                  as Yaml
import           Mockable                   (Catch, Fork, Mockable, Throw, throw)
import qualified Network.Transport.TCP      as TCP (TCPAddr (..), TCPAddrInfo (..))
import qualified Prelude
import           System.Wlog                (LoggerName, WithLogger)

import           Pos.Behavior               (BehaviorConfig (..))
import           Pos.Client.CLI.NodeOptions (CommonNodeArgs (..), NodeArgs (..))
import           Pos.Client.CLI.Options     (CommonArgs (..))
import           Pos.Client.CLI.Secrets     (updateUserSecretVSS,
                                             userSecretWithGenesisKey)
import           Pos.Core.Configuration     (HasConfiguration)
import           Pos.Core.Constants         (isDevelopment)
import           Pos.Crypto                 (VssKeyPair)
import           Pos.Launcher               (BaseParams (..), LoggingParams (..),
                                             NodeParams (..), TransportParams (..))
import           Pos.Network.CLI            (intNetworkConfigOpts)
import           Pos.Network.Types          (NetworkConfig (..), Topology (..))
import           Pos.Ssc.GodTossing         (GtParams (..))
import           Pos.Ssc.GodTossing.Configuration (HasGtConfiguration)
import           Pos.Update.Params                (UpdateParams (..))
import           Pos.Util.UserSecret              (peekUserSecret)

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

gtSscParams :: CommonNodeArgs -> VssKeyPair -> BehaviorConfig -> GtParams
gtSscParams CommonNodeArgs {..} vssSK BehaviorConfig{..} =
    GtParams
    { gtpSscEnabled = True
    , gtpVssKeyPair = vssSK
    , gtpBehavior   = bcGtBehavior
    }

getKeyfilePath :: CommonNodeArgs -> FilePath
getKeyfilePath CommonNodeArgs {..}
    | isDevelopment = case devSpendingGenesisI of
          Nothing -> keyfilePath
          Just i  -> "node-" ++ show i ++ "." ++ keyfilePath
    | otherwise = keyfilePath


getNodeParams ::
       ( MonadIO m
       , WithLogger m
       , MonadThrow m
       , Mockable Fork m
       , Mockable Catch m
       , Mockable Throw m
       , HasConfiguration
       , HasGtConfiguration
       )
    => CommonNodeArgs
    -> NodeArgs
    -> m NodeParams
getNodeParams cArgs@CommonNodeArgs{..} NodeArgs{..} = do
    (primarySK, userSecret) <-
        userSecretWithGenesisKey cArgs =<<
            updateUserSecretVSS cArgs =<<
                peekUserSecret (getKeyfilePath cArgs)
    npNetworkConfig <- intNetworkConfigOpts networkConfigOpts
    npTransport <- getTransportParams cArgs npNetworkConfig
    npBehaviorConfig <- case behaviorConfigPath of
        Nothing -> pure def
        Just fp -> either throw pure =<< liftIO (Yaml.decodeFileEither fp)
    pure NodeParams
        { npDbPathM = dbPath
        , npRebuildDb = rebuildDB
        , npSecretKey = primarySK
        , npUserSecret = userSecret
        , npBaseParams = getBaseParams "node" cArgs
        , npJLFile = jlPath
        , npReportServers = reportServers commonArgs
        , npUpdateParams = UpdateParams
            { upUpdatePath    = updateLatestPath
            , upUpdateWithPkg = updateWithPackage
            , upUpdateServers = updateServers commonArgs
            }
        , npUseNTP = not noNTP
        , npEnableMetrics = enableMetrics
        , npEkgParams = ekgParams
        , npStatsdParams = statsdParams
        , ..
        }

data NetworkTransportMisconfiguration =

      -- | A bind address was not given.
      MissingBindAddress

      -- | An external address was not given.
    | MissingExternalAddress

      -- | An address was given when one was not expected (behind NAT).
    | UnnecessaryAddress

instance Show NetworkTransportMisconfiguration where
    show MissingBindAddress     = "No network bind address given. Use the --listen option."
    show MissingExternalAddress = "No external network address given. Use the --address option."
    show UnnecessaryAddress     = "Network address given when none was expected. Remove the --listen and --address options."

instance Exception NetworkTransportMisconfiguration

getTransportParams :: ( Mockable Throw m ) => CommonNodeArgs -> NetworkConfig kademlia -> m TransportParams
getTransportParams args networkConfig = case ncTopology networkConfig of
    -- Behind-NAT topology claims no address for the transport, and also
    -- throws an exception if the --listen parameter is given, to avoid
    -- confusion: if a user gives a --listen parameter then they probably
    -- think the program will bind a socket.
    TopologyBehindNAT{} -> do
        _ <- whenJust (bindAddress args) (const (throw UnnecessaryAddress))
        _ <- whenJust (externalAddress args) (const (throw UnnecessaryAddress))
        return $ TransportParams { tpTcpAddr = TCP.Unaddressable }
    _ -> do
        (bindHost, bindPort) <- maybe (throw MissingBindAddress) return (bindAddress args)
        (externalHost, externalPort) <- maybe (throw MissingExternalAddress) return (externalAddress args)
        let tcpHost = BS8.unpack bindHost
            tcpPort = show bindPort
            tcpMkExternal = const (BS8.unpack externalHost, show externalPort)
        return $ TransportParams { tpTcpAddr = TCP.Addressable (TCP.TCPAddrInfo tcpHost tcpPort tcpMkExternal) }
