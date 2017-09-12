-- | Getter params from Args

module Pos.Client.CLI.Params
       ( loggingParams
       , getBaseParams
       , getKeyfilePath
       , getNodeParams
       , gtSscParams
       ) where

import           Universum

import           Data.Default               (def)
import qualified Data.Yaml                  as Yaml
import           Mockable                   (Catch, Fork, Mockable, Throw, throw)
import           System.Wlog                (LoggerName, WithLogger)

import           Pos.Behavior               (BehaviorConfig (..))
import           Pos.Client.CLI.NodeOptions (CommonNodeArgs (..), NodeArgs (..))
import           Pos.Client.CLI.Options     (CommonArgs (..))
import           Pos.Client.CLI.Secrets     (updateUserSecretVSS,
                                             userSecretWithGenesisKey)
import           Pos.Constants              (isDevelopment)
import           Pos.Core.Types             (Timestamp (..))
import           Pos.Crypto                 (VssKeyPair)
import           Pos.Genesis                (devBalancesDistr, devGenesisContext,
                                             genesisContextProductionM)
import           Pos.Launcher               (BaseParams (..), LoggingParams (..),
                                             NodeParams (..))
import           Pos.Network.CLI            (intNetworkConfigOpts)
import           Pos.Ssc.GodTossing         (GtParams (..))
import           Pos.Update.Params          (UpdateParams (..))
import           Pos.Util.UserSecret        (peekUserSecret)

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
       )
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
    npBehaviorConfig <- case behaviorConfigPath of
        Nothing -> pure def
        Just fp -> either throw pure =<< liftIO (Yaml.decodeFileEither fp)
    let devBalanceDistr =
            devBalancesDistr
                (flatDistr commonArgs)
                (richPoorDistr commonArgs)
                (expDistr commonArgs)
    npGenesisCtx <- if isDevelopment then pure (devGenesisContext devBalanceDistr)
                    else genesisContextProductionM
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
        , npUseNTP = not noNTP
        , npEnableMetrics = enableMetrics
        , npEkgParams = ekgParams
        , npStatsdParams = statsdParams
        , ..
        }
