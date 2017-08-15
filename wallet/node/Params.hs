-- | Getter params from Args

module Params
       ( getNodeParams
       ) where

import           Universum

import           Mockable              (Fork, Mockable)
import           System.Wlog           (WithLogger)

import qualified Pos.CLI               as CLI
import           Pos.Constants         (isDevelopment)
import           Pos.Core.Types        (Timestamp (..))
import           Pos.Genesis           (GenesisContext (..), devAddrDistr, devStakesDistr,
                                        genesisContextProduction, genesisUtxo)
import           Pos.Launcher          (NodeParams (..))
import           Pos.Network.CLI       (intNetworkConfigOpts)
import           Pos.Security          (SecurityParams (..))
import           Pos.Update.Params     (UpdateParams (..))
import           Pos.Util.UserSecret   (peekUserSecret)

import           Pos.Client.CLI.NodeOptions  (CommonNodeArgs (..))

import           Pos.Client.CLI.Params  (getBaseParams, getKeyfilePath, getTransportParams)
import           Pos.Client.CLI.Secrets (updateUserSecretVSS, userSecretWithGenesisKey)

getNodeParams ::
       (MonadIO m, MonadFail m, MonadThrow m, WithLogger m, Mockable Fork m)
    => CommonNodeArgs
    -> Timestamp
    -> m NodeParams
getNodeParams args@CommonNodeArgs{..} systemStart = do
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
            { spAttackTypes   = []
            , spAttackTargets = []
            }
        , npUseNTP = not noNTP
        , npEnableMetrics = enableMetrics
        , npEkgParams = ekgParams
        , npStatsdParams = statsdParams
        , ..
        }
