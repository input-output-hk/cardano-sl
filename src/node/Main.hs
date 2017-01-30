{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Main where

import           Control.Lens          (_head)
import           Data.List             ((!!))
import           Data.Maybe            (fromJust)
import           Data.Proxy            (Proxy (..))
import           Mockable              (Production)
import           System.Wlog           (LoggerName)
import           Universum

import           Pos.Binary            ()
import qualified Pos.CLI               as CLI
import           Pos.Constants         (staticSysStart)
import           Pos.Crypto            (SecretKey, VssKeyPair, keyGen, vssKeyGen)
import           Pos.Util.TimeWarp     (sec)
#ifdef DEV_MODE
import           Pos.Genesis           (genesisSecretKeys)
#else
import           Pos.Genesis           (genesisStakeDistribution)
#endif
import           Pos.Genesis           (genesisUtxo)
import           Pos.Launcher          (BaseParams (..), LoggingParams (..),
                                        NodeParams (..), RealModeResources,
                                        bracketResources, runNodeProduction, runNodeStats,
                                        runTimeLordReal, runTimeSlaveReal, stakesDistr)
#ifdef DEV_MODE
import           Pos.Ssc.GodTossing    (genesisVssKeyPairs)
#endif
import           Pos.Ssc.Class         (SscListenersClass)
import           Pos.Ssc.GodTossing    (GtParams (..), SscGodTossing)
import           Pos.Ssc.NistBeacon    (SscNistBeacon)
import           Pos.Ssc.SscAlgo       (SscAlgo (..))
import           Pos.Types             (Timestamp (Timestamp))
import           Pos.Util              (inAssertMode, mappendPair)
import           Pos.Util.BackupPhrase (keysFromPhrase)
import           Pos.Util.UserSecret   (UserSecret, peekUserSecret, usKeys, usVss,
                                        writeUserSecret)

#ifdef WITH_WEB
import           Pos.Ssc.Class         (SscConstraint)
import           Pos.Web               (serveWebBase, serveWebGT)
import           Pos.WorkMode          (WorkMode)
#ifdef WITH_WALLET
import           Node                  (hoistSendActions)
import           Pos.Communication     (OutSpecs, Worker, worker)
import           Pos.Statistics        (getNoStatsT, getStatsMap, runStatsT')
import           Pos.Wallet.Web        (walletServeWebFull, walletServerOuts)
import           Pos.WorkMode          (ProductionMode, RawRealMode, StatsMode)
#endif
#endif

import           NodeOptions           (Args (..), getNodeOptions)

getSystemStart
    :: SscListenersClass ssc
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
    systemStart <- case CLI.sscAlgo commonArgs of
                       GodTossingAlgo -> getSystemStart (Proxy :: Proxy SscGodTossing) res args
                       NistBeaconAlgo -> getSystemStart (Proxy :: Proxy SscNistBeacon) res args
    currentParams <- getNodeParams args systemStart
    let vssSK = fromJust $ npUserSecret currentParams ^. usVss
        gtParams = gtSscParams args vssSK
#ifdef WITH_WEB
        currentPlugins :: (SscConstraint ssc, WorkMode ssc m) => [m ()]
        currentPlugins = plugins args
        currentPluginsGT :: (WorkMode SscGodTossing m) => [m ()]
        currentPluginsGT = pluginsGT args
#else
        currentPlugins :: [a]
        currentPlugins = []
        currentPluginsGT :: [a]
        currentPluginsGT = []
#endif
    putText $ "Running using " <> show (CLI.sscAlgo commonArgs)
    putText $ "If stats is on: " <> show enableStats
    case (enableStats, CLI.sscAlgo commonArgs) of
        (True, GodTossingAlgo) ->
            runNodeStats @SscGodTossing res (convPlugins currentPluginsGT `mappendPair` walletStats args) currentParams gtParams
        (True, NistBeaconAlgo) ->
            runNodeStats @SscNistBeacon res (convPlugins currentPlugins `mappendPair`  walletStats args) currentParams ()
        (False, GodTossingAlgo) ->
            runNodeProduction @SscGodTossing res (convPlugins currentPluginsGT `mappendPair` walletProd args) currentParams gtParams
        (False, NistBeaconAlgo) ->
            runNodeProduction @SscNistBeacon res (convPlugins currentPlugins `mappendPair` walletProd args) currentParams ()
  where
    convPlugins = (,mempty) . map const

#ifdef DEV_MODE
userSecretWithGenesisKey
    :: (MonadIO m, MonadFail m) => Args -> UserSecret -> m (SecretKey, UserSecret)
userSecretWithGenesisKey Args {..} userSecret = case spendingGenesisI of
    Nothing -> fetchPrimaryKey userSecret
    Just i -> do
        let sk = genesisSecretKeys !! i
            us = userSecret & usKeys %~ (sk :) . filter (/= sk)
        writeUserSecret us
        return (sk, us)

getKeyfilePath :: Args -> FilePath
getKeyfilePath Args {..} =
    maybe keyfilePath (\i -> "node-" ++ show i ++ "." ++ keyfilePath) spendingGenesisI

updateUserSecretVSS
    :: (MonadIO m, MonadFail m) => Args -> UserSecret -> m UserSecret
updateUserSecretVSS Args {..} us = case vssGenesisI of
    Just i  -> return $ us & usVss .~ Just (genesisVssKeyPairs !! i)
    Nothing -> fillUserSecretVSS us
#else
userSecretWithGenesisKey
    :: (MonadIO m, MonadFail m) => Args -> UserSecret -> m (SecretKey, UserSecret)
userSecretWithGenesisKey _ = fetchPrimaryKey

getKeyfilePath :: Args -> FilePath
getKeyfilePath = keyfilePath

updateUserSecretVSS
    :: (MonadIO m, MonadFail m) => Args -> UserSecret -> m UserSecret
updateUserSecretVSS _ = fillUserSecretVSS
#endif

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

processUserSecret
    :: (MonadIO m, MonadFail m)
    => Args -> UserSecret -> m (SecretKey, UserSecret)
processUserSecret args@Args {..} userSecret = case backupPhrase of
    Nothing -> updateUserSecretVSS args userSecret >>= userSecretWithGenesisKey args
    Just ph -> do
        let (sk, vss) = keysFromPhrase ph
            us = userSecret & usKeys .~ [sk] & usVss .~ Just vss
        writeUserSecret us
        return (sk, us)

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
        , npAttackTypes = maliciousEmulationAttacks
        , npAttackTargets = maliciousEmulationTargets
        , npPropagation = not (CLI.disablePropagation commonArgs)
        , npUpdatePath = updateLatestPath
        , npUpdateWithPkg = updateWithPackage
        }

gtSscParams :: Args -> VssKeyPair -> GtParams
gtSscParams Args {..} vssSK =
    GtParams
    {
      gtpRebuildDb  = rebuildDB
    , gtpSscEnabled = True
    , gtpVssKeyPair = vssSK
    }

#ifdef WITH_WEB
plugins :: (SscConstraint ssc, WorkMode ssc m) => Args -> [m ()]
plugins Args {..}
    | enableWeb = [serveWebBase webPort]
    | otherwise = []
#endif

#ifdef WITH_WEB
pluginsGT :: (WorkMode SscGodTossing m) => Args -> [m ()]
pluginsGT Args {..}
    | enableWeb = [serveWebGT webPort]
    | otherwise = []
#endif

#if defined WITH_WEB && defined WITH_WALLET
walletServe
    :: SscConstraint ssc
    => Args
    -> ([Worker (RawRealMode ssc)], OutSpecs)
walletServe Args {..} =
    if enableWallet
    then first pure $ worker walletServerOuts $ \sendActions ->
            walletServeWebFull sendActions walletDebug walletDbPath
                                           walletRebuildDb walletPort
    else ([], mempty)

walletProd
    :: SscConstraint ssc
    => Args
    -> ([Worker (ProductionMode ssc)], OutSpecs)
walletProd = first (map liftPlugin) . walletServe
  where
    liftPlugin = \p sa -> lift . p $ hoistSendActions getNoStatsT lift sa

walletStats
    :: SscConstraint ssc
    => Args
    -> ([Worker (StatsMode ssc)], OutSpecs)
walletStats = first (map liftPlugin) . walletServe
  where
    liftPlugin = \p sa -> do
        s <- getStatsMap
        lift . p $ hoistSendActions (runStatsT' s) lift sa
#else
walletProd, walletStats :: Args -> [a]
walletProd _ = []
walletStats _ = []
#endif

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
    args <- getNodeOptions
    bracketResources (baseParams "node" args) (action args)
