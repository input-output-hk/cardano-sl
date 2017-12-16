-- | Getter params from Args

module Pos.Client.CLI.Params
       ( loggingParams
       , getBaseParams
       , getKeyfilePath
       , getNodeParams
       , gtSscParams
       ) where

import           Universum

import           Data.Default (def)
import qualified Data.Yaml as Yaml
import           Mockable (Catch, Fork, Mockable, Throw, throw)
import           System.Wlog (LoggerName, WithLogger)

import           Pos.Behavior (BehaviorConfig (..))
import           Pos.Client.CLI.NodeOptions (CommonNodeArgs (..), NodeArgs (..))
import           Pos.Client.CLI.Options (CommonArgs (..))
import           Pos.Client.CLI.Secrets (prepareUserSecret)
import           Pos.Core.Configuration (HasConfiguration)
import           Pos.Crypto (VssKeyPair)
import           Pos.Launcher.Param (BaseParams (..), LoggingParams (..), NodeParams (..))
import           Pos.Network.CLI (intNetworkConfigOpts)
import           Pos.Ssc (SscParams (..))
import           Pos.Ssc.Configuration (HasSscConfiguration)
import           Pos.Update.Params (UpdateParams (..))
import           Pos.Util.UserSecret (peekUserSecret)

loggingParams :: LoggerName -> CommonNodeArgs -> LoggingParams
loggingParams defaultName CommonNodeArgs{..} =
    LoggingParams
    { lpHandlerPrefix = logPrefix commonArgs
    , lpConfigPath    = logConfig commonArgs
    , lpDefaultName   = defaultName
    , lpConsoleLog    = Nothing -- no override by default
    }

getBaseParams :: LoggerName -> CommonNodeArgs -> BaseParams
getBaseParams defaultLoggerName args@CommonNodeArgs {..} =
    BaseParams { bpLoggingParams = loggingParams defaultLoggerName args }

gtSscParams :: CommonNodeArgs -> VssKeyPair -> BehaviorConfig -> SscParams
gtSscParams CommonNodeArgs {..} vssSK BehaviorConfig{..} =
    SscParams
    { spSscEnabled = True
    , spVssKeyPair = vssSK
    , spBehavior   = bcSscBehavior
    }

getKeyfilePath :: CommonNodeArgs -> FilePath
getKeyfilePath CommonNodeArgs {..}
    = case devGenesisSecretI of
          Nothing -> keyfilePath
          Just i  -> "node-" ++ show i ++ "." ++ keyfilePath

getNodeParams ::
       ( MonadIO m
       , WithLogger m
       , MonadThrow m
       , Mockable Fork m
       , Mockable Catch m
       , Mockable Throw m
       , HasConfiguration
       , HasSscConfiguration
       )
    => LoggerName
    -> CommonNodeArgs
    -> NodeArgs
    -> m NodeParams
getNodeParams defaultLoggerName cArgs@CommonNodeArgs{..} NodeArgs{..} = do
    (primarySK, userSecret) <-
        prepareUserSecret cArgs =<< peekUserSecret (getKeyfilePath cArgs)
    npNetworkConfig <- intNetworkConfigOpts networkConfigOpts
    npBehaviorConfig <- case behaviorConfigPath of
        Nothing -> pure def
        Just fp -> either throw pure =<< liftIO (Yaml.decodeFileEither fp)
    pure NodeParams
        { npDbPathM = dbPath
        , npRebuildDb = rebuildDB
        , npSecretKey = primarySK
        , npUserSecret = userSecret
        , npBaseParams = getBaseParams defaultLoggerName cArgs
        , npJLFile = jlPath
        , npReportServers = reportServers commonArgs
        , npUpdateParams = UpdateParams
            { upUpdatePath    = updateLatestPath
            , upUpdateWithPkg = updateWithPackage
            , upUpdateServers = updateServers commonArgs
            }
        , npUseNTP = not noNTP
        , npRoute53Params = route53Params
        , npEnableMetrics = enableMetrics
        , npEkgParams = ekgParams
        , npStatsdParams = statsdParams
        , ..
        }
