{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Main where

import           Data.Maybe          (fromJust)
import           Mockable            (Production, currentTime)
import           System.Wlog         (LoggerName, HasLoggerName, CanLog, logInfo)
import           Formatting          ((%), sformat, shown)
import           Universum

import           Pos.Binary          ()
import           Pos.Core.Types      (Timestamp (..))
import qualified Pos.CLI             as CLI
import           Pos.Crypto          (SecretKey, VssKeyPair, keyGen, vssKeyGen)
#ifndef DEV_MODE
import           Pos.Genesis         (genesisStakeDistribution)
#endif
import           Pos.Genesis         (genesisUtxo)
import           Pos.Launcher        (BaseParams (..), LoggingParams (..),
                                      NodeParams (..), RealModeResources,
                                      bracketResources, runNodeProduction,
                                      stakesDistr)
import           Pos.Ssc.GodTossing  (GtParams (..), SscGodTossing)
import           Pos.Types           (Timestamp (Timestamp))
import           Pos.Update          (UpdateParams (..))
import           Pos.Util            (inAssertMode, mconcatPair)
import           Pos.Util.UserSecret (UserSecret, peekUserSecret, usPrimKey, usVss,
                                      writeUserSecret)

import           Pos.Explorer.Socket (NotifierSettings (..))
import           Pos.Explorer.Web    (explorerPlugin, notifierPlugin)

import           ExplorerOptions     (Args (..), getExplorerOptions)

loggingParams :: LoggerName -> Args -> LoggingParams
loggingParams tag Args{..} =
    LoggingParams
    { lpHandlerPrefix = CLI.logPrefix commonArgs
    , lpConfigPath    = CLI.logConfig commonArgs
    , lpRunnerTag     = tag
    , lpEkgPort       = monitorPort
    }

baseParams :: LoggerName -> Args -> BaseParams
baseParams loggingTag args@Args {..} =
    BaseParams
    { bpLoggingParams = loggingParams loggingTag args
    , bpBindAddress = ipPort
    , bpPublicHost = publicHost
    , bpDHTPeers = CLI.dhtPeers commonArgs
    , bpDHTKey = dhtKey
    , bpDHTExplicitInitial = CLI.dhtExplicitInitial commonArgs
    , bpKademliaDump = kademliaDumpPath
    }

action :: Args -> RealModeResources -> Production ()
action args@Args {..} res = do
    let systemStart = CLI.sysStart commonArgs
    logInfo $ sformat ("System start time is " % shown) systemStart
    t <- currentTime
    logInfo $ sformat ("Current time is " % shown) (Timestamp t)

    currentParams <- getNodeParams args systemStart

    let vssSK = fromJust $ npUserSecret currentParams ^. usVss
        gtParams = gtSscParams args vssSK

    putText "Running using GodTossing"
    let plugins = mconcatPair
            [ explorerPlugin webPort
            , notifierPlugin NotifierSettings{ nsPort = notifierPort }
            ]
    runNodeProduction @SscGodTossing res plugins currentParams gtParams

userSecretWithGenesisKey
    :: (MonadIO m, MonadFail m) => Args -> UserSecret -> m (SecretKey, UserSecret)
userSecretWithGenesisKey _ = fetchPrimaryKey

getKeyfilePath :: Args -> FilePath
getKeyfilePath = keyfilePath

updateUserSecretVSS
    :: (MonadIO m, MonadFail m) => Args -> UserSecret -> m UserSecret
updateUserSecretVSS _ = fillUserSecretVSS

fetchPrimaryKey :: (MonadIO m, MonadFail m) => UserSecret -> m (SecretKey, UserSecret)
fetchPrimaryKey userSecret = case userSecret ^. usPrimKey of
    Just sk -> return (sk, userSecret)
    Nothing -> do
        putText "Found no signing keys in keyfile, generating random one..."
        sk <- snd <$> keyGen
        let us = userSecret & usPrimKey .~ Just sk
        writeUserSecret us
        return (sk, us)

fillUserSecretVSS :: (MonadIO m, MonadFail m) => UserSecret -> m UserSecret
fillUserSecretVSS userSecret = case userSecret ^. usVss of
    Just _  -> return userSecret
    Nothing -> do
        putText "Found no VSS keypair in keyfile, generating random one..."
        vss <- vssKeyGen
        let us = userSecret & usVss .~ Just vss
        writeUserSecret us
        return us

getNodeParams :: (MonadIO m, MonadFail m, MonadThrow m, CanLog m, HasLoggerName m) => Args -> Timestamp -> m NodeParams
getNodeParams args@Args {..} systemStart = do
    (primarySK, userSecret) <-
        userSecretWithGenesisKey args =<<
        updateUserSecretVSS args =<<
        peekUserSecret (getKeyfilePath args)

    return NodeParams
        { npDbPathM = dbPath
        , npRebuildDb = rebuildDB
        , npSecretKey = primarySK
        , npUserSecret = userSecret
        , npSystemStart = systemStart
        , npBaseParams = baseParams "node" args
        , npCustomUtxo =
                genesisUtxo $
#ifdef DEV_MODE
                stakesDistr (CLI.flatDistr commonArgs)
                            (CLI.bitcoinDistr commonArgs)
                            (CLI.richPoorDistr commonArgs)
                            (CLI.expDistr commonArgs)
#else
                genesisStakeDistribution
#endif
        , npTimeLord = timeLord
        , npJLFile = jlPath
        , npAttackTypes = []
        , npAttackTargets = []
        , npPropagation = not (CLI.disablePropagation commonArgs)
        , npUpdateParams = UpdateParams
            { upUpdatePath = "explorer-update"
            , upUpdateWithPkg = True
            , upUpdateServers = CLI.updateServers commonArgs
            }
        , npReportServers = CLI.reportServers commonArgs
        }

gtSscParams :: Args -> VssKeyPair -> GtParams
gtSscParams Args {..} vssSK =
    GtParams
    { gtpSscEnabled = True
    , gtpVssKeyPair = vssSK
    }

printFlags :: IO ()
printFlags = do
#ifdef DEV_MODE
    putText "[Attention] We are in DEV mode"
#else
    putText "[Attention] We are in PRODUCTION mode"
#endif
    inAssertMode $ putText "Asserts are ON"

main :: IO ()
main = do
    printFlags
    args <- getExplorerOptions
    bracketResources (baseParams "node" args) (action args)
