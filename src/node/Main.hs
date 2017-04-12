{-# LANGUAGE CPP #-}

module Main where

import           Data.List             ((!!))
import           Data.Maybe            (fromJust)
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Data.Time.Units       (toMicroseconds)
import           Formatting            (sformat, shown, (%))
import           Mockable              (Production, currentTime)
import           Node                  (hoistSendActions)
import           Serokell.Util         (sec)
import           System.Wlog           (LoggerName, WithLogger, logInfo)
import           Universum

import           Pos.Binary            ()
import qualified Pos.CLI               as CLI
import           Pos.Communication     (ActionSpec (..), OutSpecs, WorkerSpec, worker)
import           Pos.Constants         (isDevelopment)
import           Pos.Core.Types        (Timestamp (..))
import           Pos.Crypto            (SecretKey, VssKeyPair, keyGen, noPassEncrypt,
                                        vssKeyGen)
import           Pos.Genesis           (genesisDevSecretKeys, genesisStakeDistribution,
                                        genesisUtxo)
import           Pos.Launcher          (BaseParams (..), LoggingParams (..),
                                        NodeParams (..), RealModeResources,
                                        bracketResources, runNodeProduction, runNodeStats,
                                        stakesDistr)
import           Pos.Shutdown          (triggerShutdown)
import           Pos.Ssc.Class         (SscConstraint)
import           Pos.Ssc.GodTossing    (GtParams (..), SscGodTossing,
                                        genesisDevVssKeyPairs)
import           Pos.Ssc.NistBeacon    (SscNistBeacon)
import           Pos.Ssc.SscAlgo       (SscAlgo (..))
import           Pos.Statistics        (getNoStatsT, getStatsMap, runStatsT')
import           Pos.Update.Context    (ucUpdateSemaphore)
import           Pos.Update.Params     (UpdateParams (..))
import           Pos.Util              (inAssertMode, mappendPair)
import           Pos.Util.BackupPhrase (keysFromPhrase)
import           Pos.Util.UserSecret   (UserSecret, peekUserSecret, usKeys, usPrimKey,
                                        usVss, writeUserSecret)
import           Pos.WorkMode          (ProductionMode, RawRealMode, StatsMode)
#ifdef WITH_WEB
import           Pos.Web               (serveWebBase, serveWebGT)
import           Pos.WorkMode          (WorkMode)
#ifdef WITH_WALLET
import           Pos.Wallet.Web        (walletServeWebFull, walletServerOuts)
#endif
#endif
import           Pos.Util.Context      (askContext)

import           NodeOptions           (Args (..), getNodeOptions)

loggingParams :: LoggerName -> Args -> LoggingParams
loggingParams tag Args{..} =
    LoggingParams
    { lpHandlerPrefix = CLI.logPrefix commonArgs
    , lpConfigPath    = CLI.logConfig commonArgs
    , lpRunnerTag = tag
    , lpEkgPort = monitorPort
    }

baseParams :: LoggerName -> Args -> IO BaseParams
baseParams loggingTag args@Args {..} = do
    filePeers <- maybe (return []) CLI.readPeersFile
                     (CLI.dhtPeersFile commonArgs)
    let allPeers = CLI.dhtPeers commonArgs ++ filePeers
    return $ BaseParams
        { bpLoggingParams = loggingParams loggingTag args
        , bpIpPort = ipPort
        , bpDHTPeers = allPeers
        , bpDHTKey = dhtKey
        , bpDHTExplicitInitial = CLI.dhtExplicitInitial commonArgs
        , bpKademliaDump = kademliaDumpPath
        }

getNodeSystemStart :: (MonadIO m) => Timestamp -> m Timestamp
getNodeSystemStart cliOrConfigSystemStart
  | cliOrConfigSystemStart >= 1400000000 =
    -- UNIX time 1400000000 is Tue, 13 May 2014 16:53:20 GMT.
    -- It was chosen arbitrarily as some date far enough in the past.
    -- See CSL-983 for more information.
    pure cliOrConfigSystemStart
  | otherwise = do
    let frameLength = timestampToSeconds cliOrConfigSystemStart
    currentPOSIXTime <- liftIO $ round <$> getPOSIXTime
    -- The whole timeline is split into frames, with the first frame starting
    -- at UNIX epoch start. We're looking for a time `t` which would be in the
    -- middle of the same frame as the current UNIX time.
    let currentFrame = currentPOSIXTime `div` frameLength
        t = currentFrame * frameLength + (frameLength `div` 2)
    pure $ Timestamp $ sec $ fromIntegral t
  where
    timestampToSeconds :: Timestamp -> Integer
    timestampToSeconds = (`div` 1000000) . toMicroseconds . getTimestamp

action :: Args -> RealModeResources -> Production ()
action args@Args {..} res = do
    systemStart <- getNodeSystemStart $ CLI.sysStart commonArgs
    logInfo $ sformat ("System start time is " % shown) systemStart
    t <- currentTime
    logInfo $ sformat ("Current time is " % shown) (Timestamp t)
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
            runNodeStats @SscGodTossing res
                (convPlugins currentPluginsGT `mappendPair` walletStats args)
                currentParams gtParams
        (True, NistBeaconAlgo) ->
            runNodeStats @SscNistBeacon res
                (convPlugins currentPlugins `mappendPair`  walletStats args)
                currentParams ()
        (False, GodTossingAlgo) ->
            runNodeProduction @SscGodTossing res
                (convPlugins currentPluginsGT `mappendPair` walletProd args)
                currentParams gtParams
        (False, NistBeaconAlgo) ->
            runNodeProduction @SscNistBeacon res
                (convPlugins currentPlugins `mappendPair` walletProd args)
                currentParams ()
  where
    convPlugins = (,mempty) . map (\act -> ActionSpec $ \__vI __sA -> act)

userSecretWithGenesisKey
    :: (MonadIO m, MonadFail m) => Args -> UserSecret -> m (SecretKey, UserSecret)
userSecretWithGenesisKey Args{..} userSecret
    | isDevelopment = case devSpendingGenesisI of
          Nothing -> fetchPrimaryKey userSecret
          Just i -> do
              let sk = genesisDevSecretKeys !! i
                  us = userSecret
                       & usPrimKey .~ Just sk
                       & usKeys %~ (noPassEncrypt sk :)
              writeUserSecret us
              return (sk, us)
    | otherwise = fetchPrimaryKey userSecret

getKeyfilePath :: Args -> FilePath
getKeyfilePath Args {..}
    | isDevelopment = case devSpendingGenesisI of
          Nothing -> keyfilePath
          Just i  -> "node-" ++ show i ++ "." ++ keyfilePath
    | otherwise = keyfilePath

updateUserSecretVSS
    :: (MonadIO m, MonadFail m) => Args -> UserSecret -> m UserSecret
updateUserSecretVSS Args{..} us
    | isDevelopment = case devVssGenesisI of
          Nothing -> fillUserSecretVSS us
          Just i  -> return $ us & usVss .~ Just (genesisDevVssKeyPairs !! i)
    | otherwise = fillUserSecretVSS us

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

processUserSecret
    :: (MonadIO m, MonadFail m)
    => Args -> UserSecret -> m (SecretKey, UserSecret)
processUserSecret args@Args {..} userSecret = case backupPhrase of
    Nothing -> updateUserSecretVSS args userSecret >>= userSecretWithGenesisKey args
    Just ph -> do
        (sk, vss) <- either keyFromPhraseFailed pure $ keysFromPhrase ph
        let us = userSecret & usPrimKey .~ Just sk & usVss .~ Just vss
        writeUserSecret us
        return (sk, us)
  where
    keyFromPhraseFailed msg = fail $ "Key creation from phrase failed: " <> show msg

getNodeParams
    :: (MonadIO m, MonadFail m, MonadThrow m, WithLogger m)
    => Args -> Timestamp -> m NodeParams
getNodeParams args@Args {..} systemStart = do
    (primarySK, userSecret) <-
        userSecretWithGenesisKey args =<<
        updateUserSecretVSS args =<<
        peekUserSecret (getKeyfilePath args)
    params <- liftIO $ baseParams "node" args
    return NodeParams
        { npDbPathM = dbPath
        , npRebuildDb = rebuildDB
        , npSecretKey = primarySK
        , npUserSecret = userSecret
        , npSystemStart = systemStart
        , npBaseParams = params
        , npCustomUtxo = genesisUtxo $
              if isDevelopment
                  then stakesDistr (CLI.flatDistr commonArgs)
                                   (CLI.bitcoinDistr commonArgs)
                                   (CLI.richPoorDistr commonArgs)
                                   (CLI.expDistr commonArgs)
                  else genesisStakeDistribution
        , npTimeLord = timeLord
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
        }

gtSscParams :: Args -> VssKeyPair -> GtParams
gtSscParams Args {..} vssSK =
    GtParams
    { gtpSscEnabled = True
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

updateTriggerWorker
    :: SscConstraint ssc
    => ([WorkerSpec (RawRealMode ssc)], OutSpecs)
updateTriggerWorker = first pure $ worker mempty $ \_ -> do
    logInfo "Update trigger worker is locked"
    void $ liftIO . takeMVar =<< askContext ucUpdateSemaphore
    triggerShutdown

walletServe
    :: SscConstraint ssc
    => Args
    -> ([WorkerSpec (RawRealMode ssc)], OutSpecs)
#if defined WITH_WEB && defined WITH_WALLET
walletServe Args {..} =
    if enableWallet
    then first pure $ worker walletServerOuts $ \sendActions ->
            walletServeWebFull sendActions walletDebug walletDbPath
                                           walletRebuildDb walletPort
    else updateTriggerWorker
#else
walletServe _ = updateTriggerWorker
#endif

walletProd
    :: SscConstraint ssc
    => Args
    -> ([WorkerSpec (ProductionMode ssc)], OutSpecs)
walletProd = first (map liftPlugin) . walletServe
  where
    liftPlugin (ActionSpec p) = ActionSpec $ \vI sa ->
        lift . p vI $ hoistSendActions getNoStatsT lift sa

walletStats
    :: SscConstraint ssc
    => Args
    -> ([WorkerSpec (StatsMode ssc)], OutSpecs)
walletStats = first (map liftPlugin) . walletServe
  where
    liftPlugin (ActionSpec p) = ActionSpec $ \vI sa -> do
        s <- getStatsMap
        lift . p vI $ hoistSendActions (runStatsT' s) lift sa

printFlags :: IO ()
printFlags = do
    if isDevelopment
        then putText "[Attention] We are in DEV mode"
        else putText "[Attention] We are in PRODUCTION mode"
#ifdef WITH_WEB
    putText "[Attention] Web-mode is on"
#endif
#ifdef WITH_WALLET
    putText "[Attention] Wallet-mode is on"
#endif
    inAssertMode $ putText "Asserts are ON"

main :: IO ()
main = do
    printFlags
    args <- getNodeOptions
    params <- baseParams "node" args
    bracketResources params (action args)
