-- | Getter params from Args

module Params
       ( gtSscParams
       , getLoggingParams
       , getNodeParams
       , getBaseParams
       ) where

import           Universum

import           Data.Default                     (def)
import           Mockable                         (Catch, Fork, Mockable, Throw)
import           System.Wlog                      (LoggerName, WithLogger)

import qualified Pos.Client.CLI                   as CLI
import           Pos.Core.Configuration           (HasConfiguration)
import           Pos.Crypto                       (VssKeyPair)
import           Pos.Launcher                     (BaseParams (..), LoggingParams (..),
                                                   NodeParams (..))
import           Pos.Network.CLI                  (intNetworkConfigOpts)
import           Pos.Ssc.GodTossing               (GtParams (..))
import           Pos.Ssc.GodTossing.Configuration (HasGtConfiguration)
import           Pos.Update.Params                (UpdateParams (..))
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
        , npRoute53Params = Nothing -- We don't want health checking for Explorer
        , npStatsdParams = statsdParams
        , ..
        }
