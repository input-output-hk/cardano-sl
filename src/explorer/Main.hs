{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Main where

import           Control.Lens        (_head)
import           Data.Maybe          (fromJust)
import           Data.Proxy          (Proxy (..))
import           Mockable            (Production)
import           System.Wlog         (LoggerName)
import           Universum

import           Pos.Binary          ()
import qualified Pos.CLI             as CLI
import           Pos.Communication   (ActionSpec (..))
import           Pos.Constants       (staticSysStart)
import           Pos.Crypto          (SecretKey, VssKeyPair, keyGen, vssKeyGen)
#ifndef DEV_MODE
import           Pos.Genesis         (genesisStakeDistribution)
#endif
import           Pos.Genesis         (genesisUtxo)
import           Pos.Launcher        (BaseParams (..), LoggingParams (..),
                                      NodeParams (..), RealModeResources,
                                      bracketResources, runNodeProduction,
                                      runTimeLordReal, runTimeSlaveReal, stakesDistr)
import           Pos.Ssc.Class       (SscConstraint)
import           Pos.Ssc.GodTossing  (GtParams (..), SscGodTossing)
import           Pos.Types           (Timestamp (Timestamp))
import           Pos.Util            (inAssertMode)
import           Pos.Util.TimeWarp   (sec)
import           Pos.Util.UserSecret (UserSecret, peekUserSecret, usKeys, usVss,
                                      writeUserSecret)
import           Pos.Web             (serveWebGT)
import           Pos.WorkMode        (ProductionMode, WorkMode)

import           ExplorerOptions     (Args (..), getExplorerOptions)

getSystemStart
    :: SscConstraint ssc
    => Proxy ssc -> RealModeResources -> Args -> Production Timestamp
getSystemStart sscProxy inst args
    | noSystemStart args > 0 = pure $ Timestamp $ sec $ noSystemStart args
    | otherwise =
        case staticSysStart of
            Nothing ->
                if timeLord args
                    then runTimeLordReal (loggingParams "time-lord" args)
                    else runTimeSlaveReal
                             sscProxy
                             inst
                             (baseParams "time-slave" args)
            Just systemStart -> return systemStart

loggingParams :: LoggerName -> Args -> LoggingParams
loggingParams tag Args{..} =
    LoggingParams
    { lpHandlerPrefix = CLI.logPrefix commonArgs
    , lpConfigPath    = CLI.logConfig commonArgs
    , lpRunnerTag = tag
    }

baseParams :: LoggerName -> Args -> BaseParams
baseParams loggingTag args@Args {..} =
    BaseParams
    { bpLoggingParams = loggingParams loggingTag args
    , bpIpPort = ipPort
    , bpDHTPeers = CLI.dhtPeers commonArgs
    , bpDHTKey = dhtKey
    , bpDHTExplicitInitial = CLI.dhtExplicitInitial commonArgs
    , bpKademliaDump = kademliaDumpPath
    }

action :: Args -> RealModeResources -> Production ()
action args@Args {..} res = do
    systemStart <- getSystemStart (Proxy :: Proxy SscGodTossing) res args
    currentParams <- getNodeParams args systemStart

    let vssSK = fromJust $ npUserSecret currentParams ^. usVss
        gtParams = gtSscParams args vssSK
        convPlugins = (,mempty) . map (\act -> ActionSpec $ \__vI __sA -> act)
        currentPluginsGT :: [ProductionMode SscGodTossing ()]
        currentPluginsGT = pluginsGT args

    putText "Running using GodTossing"
    runNodeProduction @SscGodTossing res (convPlugins currentPluginsGT) currentParams gtParams

userSecretWithGenesisKey
    :: (MonadIO m, MonadFail m) => Args -> UserSecret -> m (SecretKey, UserSecret)
userSecretWithGenesisKey _ = fetchPrimaryKey

getKeyfilePath :: Args -> FilePath
getKeyfilePath = keyfilePath

updateUserSecretVSS
    :: (MonadIO m, MonadFail m) => Args -> UserSecret -> m UserSecret
updateUserSecretVSS _ = fillUserSecretVSS

fetchPrimaryKey :: (MonadIO m, MonadFail m) => UserSecret -> m (SecretKey, UserSecret)
fetchPrimaryKey userSecret = case userSecret ^? usKeys . _head of
    Just sk -> return (sk, userSecret)
    Nothing -> do
        putText "Found no signing keys in keyfile, generating random one..."
        sk <- snd <$> keyGen
        let us = userSecret & usKeys .~ [sk]
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

getNodeParams :: (MonadIO m, MonadFail m) => Args -> Timestamp -> m NodeParams
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
                            (CLI.expDistr commonArgs)
#else
                genesisStakeDistribution
#endif
        , npTimeLord = timeLord
        , npJLFile = jlPath
        , npAttackTypes = []
        , npAttackTargets = []
        , npPropagation = not (CLI.disablePropagation commonArgs)
        , npUpdatePath = "explorer-update"
        , npUpdateWithPkg = True
        }

gtSscParams :: Args -> VssKeyPair -> GtParams
gtSscParams Args {..} vssSK =
    GtParams
    { gtpSscEnabled = True
    , gtpVssKeyPair = vssSK
    }

pluginsGT :: (WorkMode SscGodTossing m) => Args -> [m ()]
pluginsGT Args {..} = [serveWebGT webPort]

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
