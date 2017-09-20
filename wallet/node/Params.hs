-- | Getter params from Args

module Params
       ( getNodeParams
       ) where

import           Universum

import           Data.Default        (def)
import           Mockable            (Catch, Fork, Mockable, Throw)
import           System.Wlog         (WithLogger)

import           Pos.Client.CLI      (CommonNodeArgs (..))
import qualified Pos.Client.CLI      as CLI
import           Pos.Constants       (isDevelopment)
import           Pos.Core.Types      (Timestamp (..))
import           Pos.Genesis         (devBalancesDistr, devGenesisContext,
                                      genesisContextProduction)
import           Pos.Launcher        (NodeParams (..))
import           Pos.Network.CLI     (intNetworkConfigOpts)
import           Pos.Update.Params   (UpdateParams (..))
import           Pos.Util.UserSecret (peekUserSecret)

getNodeParams ::
       ( MonadIO m
       , MonadFail m
       , MonadThrow m
       , WithLogger m
       , Mockable Fork m
       , Mockable Catch m
       , Mockable Throw m
       )
    => CommonNodeArgs
    -> Timestamp
    -> m NodeParams
getNodeParams args@CommonNodeArgs{..} systemStart = do
    (primarySK, userSecret) <-
        CLI.userSecretWithGenesisKey args =<<
            CLI.updateUserSecretVSS args =<<
                peekUserSecret (CLI.getKeyfilePath args)
    npNetworkConfig <- intNetworkConfigOpts networkConfigOpts
    npTransport <- CLI.getTransportParams args npNetworkConfig
    let devBalanceDistr =
            devBalancesDistr
                (CLI.flatDistr commonArgs)
                (CLI.richPoorDistr commonArgs)
                (CLI.expDistr commonArgs)
    let npGenesisCtx
            | isDevelopment = devGenesisContext devBalanceDistr
            | otherwise = genesisContextProduction
    pure NodeParams
        { npDbPathM = dbPath
        , npRebuildDb = rebuildDB
        , npSecretKey = primarySK
        , npUserSecret = userSecret
        , npSystemStart = systemStart
        , npBaseParams = CLI.getBaseParams "node" args
        , npJLFile = jlPath
        , npReportServers = CLI.reportServers commonArgs
        , npUpdateParams = UpdateParams
            { upUpdatePath    = updateLatestPath
            , upUpdateWithPkg = updateWithPackage
            , upUpdateServers = CLI.updateServers commonArgs
            }
        , npBehaviorConfig = def
        , npUseNTP = not noNTP
        , npRoute53Params = route53Params
        , npEnableMetrics = enableMetrics
        , npEkgParams = ekgParams
        , npStatsdParams = statsdParams
        , ..
        }
