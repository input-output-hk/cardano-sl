-- | Getter params from Args

module Params
       ( loggingParams
       , getKademliaParams
       , getBaseParams
       , getNodeParams
       , gtSscParams
       , getPeersFromArgs
       ) where

import           System.Wlog         (LoggerName, WithLogger)
import           Universum

import qualified Pos.CLI             as CLI
import           Pos.Constants       (isDevelopment)
import           Pos.Core.Types      (Timestamp (..))
import           Pos.Crypto          (VssKeyPair)
import           Pos.DHT.Real        (KademliaParams (..))
import           Pos.Genesis         (genesisStakeDistribution, genesisUtxo)
import           Pos.Ssc.GodTossing  (GtParams (..))
import           Pos.Update.Params   (UpdateParams (..))
import           Pos.Util.TimeWarp   (NetworkAddress, readAddrFile)
import           Pos.Util.UserSecret (peekUserSecret)

import           NodeOptions         (Args (..))
import           Pos.Launcher        (BaseParams (..), LoggingParams (..),
                                      NodeParams (..), stakesDistr)
import           Secrets             (updateUserSecretVSS, userSecretWithGenesisKey)


loggingParams :: LoggerName -> Args -> LoggingParams
loggingParams tag Args{..} =
    LoggingParams
    { lpHandlerPrefix = CLI.logPrefix commonArgs
    , lpConfigPath    = CLI.logConfig commonArgs
    , lpRunnerTag = tag
    , lpEkgPort = monitorPort
    }

getPeersFromArgs :: Args -> IO [NetworkAddress]
getPeersFromArgs Args {..} = do
    filePeers <- maybe (return []) readAddrFile dhtPeersFile
    pure $ dhtPeersList ++ filePeers

-- | Load up the KademliaParams. It's in IO because we may have to read a
--   file to find some peers.
getKademliaParams :: Args -> IO KademliaParams
getKademliaParams args@Args{..} = do
    allPeers <- getPeersFromArgs args
    pure $ KademliaParams
                 { kpNetworkAddress  = dhtNetworkAddress
                 , kpPeers           = allPeers
                 , kpKey             = dhtKey
                 , kpExplicitInitial = dhtExplicitInitial
                 , kpDump            = kademliaDumpPath
                 , kpExternalAddress = externalAddress
                 }

getBaseParams :: LoggerName -> Args -> BaseParams
getBaseParams loggingTag args@Args {..} =
    BaseParams { bpLoggingParams = loggingParams loggingTag args }

gtSscParams :: Args -> VssKeyPair -> GtParams
gtSscParams Args {..} vssSK =
    GtParams
    { gtpSscEnabled = True
    , gtpVssKeyPair = vssSK
    }

getKeyfilePath :: Args -> FilePath
getKeyfilePath Args {..}
    | isDevelopment = case devSpendingGenesisI of
          Nothing -> keyfilePath
          Just i  -> "node-" ++ show i ++ "." ++ keyfilePath
    | otherwise = keyfilePath

getNodeParams
    :: (MonadIO m, MonadFail m, MonadThrow m, WithLogger m)
    => Args -> Timestamp -> m NodeParams
getNodeParams args@Args {..} systemStart = do
    (primarySK, userSecret) <-
        userSecretWithGenesisKey args =<<
        updateUserSecretVSS args =<<
        peekUserSecret (getKeyfilePath args)
    pure NodeParams
        { npDbPathM = dbPath
        , npRebuildDb = rebuildDB
        , npSecretKey = primarySK
        , npUserSecret = userSecret
        , npSystemStart = systemStart
        , npBaseParams = getBaseParams "node" args
        , npCustomUtxo = genesisUtxo $
              if isDevelopment
                  then stakesDistr (CLI.flatDistr commonArgs)
                                   (CLI.bitcoinDistr commonArgs)
                                   (CLI.richPoorDistr commonArgs)
                                   (CLI.expDistr commonArgs)
                  else genesisStakeDistribution
        , npJLFile = jlPath
        , npAttackTypes = maliciousEmulationAttacks
        , npAttackTargets = maliciousEmulationTargets
        , npPropagation = not (CLI.disablePropagation commonArgs)
        , npReportServers = CLI.reportServers commonArgs
        , npUpdateParams = UpdateParams
            { upUpdatePath = updateLatestPath
            , upUpdateWithPkg = updateWithPackage
            , upUpdateServers = CLI.updateServers commonArgs
            }
        , npUseNTP = not noNTP
        }
