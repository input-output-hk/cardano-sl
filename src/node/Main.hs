{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Main where

import           Control.Lens         ((%~), (.~), (^.), (^?), _head)
import qualified Data.ByteString.Lazy as LBS
import           Data.List            ((!!))
import           Data.Maybe           (fromJust)
import           Data.Proxy           (Proxy (..))
import           Mockable             (Production)
import           Node                 (SendActions, hoistSendActions)
import           System.Directory     (createDirectoryIfMissing)
import           System.FilePath      ((</>))
import           System.Wlog          (LoggerName)
import           Universum

import           Pos.Binary           (Bi, decode, encode)
import qualified Pos.CLI              as CLI
import           Pos.Communication    (BiP)
import           Pos.Constants        (RunningMode (..), runningMode)
import           Pos.Crypto           (SecretKey, VssKeyPair, vssKeyGen)
import           Pos.DHT.Model        (DHTKey, DHTNodeType (..), dhtNodeType)
import           Pos.Genesis          (genesisSecretKeys, genesisUtxo)
import           Pos.Launcher         (BaseParams (..), LoggingParams (..),
                                       NodeParams (..), RealModeResources,
                                       bracketResources, runNodeProduction, runNodeStats,
                                       runTimeLordReal, runTimeSlaveReal, stakesDistr)
import           Pos.Ssc.GodTossing   (genesisVssKeyPairs)
import           Pos.Ssc.GodTossing   (GtParams (..), SscGodTossing)
import           Pos.Ssc.NistBeacon   (SscNistBeacon)
import           Pos.Ssc.SscAlgo      (SscAlgo (..))
import           Pos.Statistics       (getNoStatsT, getStatsMap, runStatsT')
import           Pos.Types            (Timestamp)
import           Pos.Util.UserSecret  (UserSecret, getUSPath, peekUserSecret, usKeys,
                                       usVss, writeUserSecret)
import           Pos.Ssc.Class        (SscConstraint, SscListenersClass)

#ifdef WITH_WEB
import           Pos.Web              (serveWebBase, serveWebGT)
import           Pos.WorkMode         (WorkMode)
#ifdef WITH_WALLET
import           Pos.WorkMode         (ProductionMode, RawRealMode, StatsMode)

import           Pos.Wallet.Web       (walletServeWebFull)
#endif
#endif

import           NodeOptions          (Args (..), getNodeOptions)


getKey
    :: Bi key
    => Maybe key -> Maybe FilePath -> FilePath -> IO key -> IO key
getKey (Just key) _ _ _ = return key
getKey _ (Just path) _ _ = decode' path
getKey _ _ fpath gen = do
    createDirectoryIfMissing True "cardano-keys"
    decode' ("cardano-keys" </> fpath) `catch` \(_ :: SomeException) -> do
        key <- gen
        LBS.writeFile ("cardano-keys" </> fpath) $ encode key
        putStrLn $ "Generated key " ++ ("cardano-keys" </> fpath)
        return key

decode' :: Bi key => FilePath -> IO key
decode' fpath = either fail' return . decode =<< LBS.readFile fpath
  where
    fail' e = fail $ "Error reading key from " ++ fpath ++ ": " ++ e

getSystemStart :: SscListenersClass ssc => Proxy ssc -> RealModeResources -> Args -> Production Timestamp
getSystemStart sscProxy inst args  =
    case runningMode of
        Development ->
            if timeLord args
                then runTimeLordReal (loggingParams "time-lord" args)
                else runTimeSlaveReal sscProxy inst (baseParams "time-slave" args)
        Production systemStart -> return systemStart

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
    , bpPort = port
    , bpDHTPeers = CLI.dhtPeers commonArgs
    , bpDHTKeyOrType = dhtKeyOrType
    , bpDHTExplicitInitial = CLI.dhtExplicitInitial commonArgs
    }
  where
    dhtKeyOrType
        | supporterNode = maybe (Right DHTSupporter) Left dhtKey
        | otherwise = maybe (Right DHTFull) Left dhtKey

checkDhtKey :: Bool -> Maybe DHTKey -> Production ()
checkDhtKey _ Nothing = pass
checkDhtKey isSupporter (Just (dhtNodeType -> keyType))
    | keyType == Just expectedType = pass
    | otherwise =
        fail $
        case keyType of
            Just type_' -> "Id of type " ++ (show type_') ++ " supplied"
            _           -> "Id of unknown type supplied"
  where
    expectedType
        | isSupporter = DHTSupporter
        | otherwise = DHTFull

action :: Args -> RealModeResources -> Production ()
action args@Args {..} res = do
    checkDhtKey supporterNode dhtKey
    if supporterNode
        then fail "Supporter not supported" -- runSupporterReal res (baseParams "supporter" args)
        else do
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
                    runNodeStats @SscGodTossing res (map const currentPluginsGT ++ walletStats args) currentParams gtParams
                (True, NistBeaconAlgo) ->
                    runNodeStats @SscNistBeacon res (map const currentPlugins ++  walletStats args) currentParams ()
                (False, GodTossingAlgo) ->
                    runNodeProduction @SscGodTossing res (map const currentPluginsGT ++ walletProd args) currentParams gtParams
                (False, NistBeaconAlgo) ->
                    runNodeProduction @SscNistBeacon res (map const currentPlugins ++ walletProd args) currentParams ()

#ifdef DEV_MODE
userSecretWithGenesisKey
    :: (MonadIO m, MonadFail m) => Args -> UserSecret -> m (SecretKey, UserSecret)
userSecretWithGenesisKey Args {..} userSecret = case spendingGenesisI of
    Nothing -> (, userSecret) <$> fetchPrimaryKey userSecret
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
userSecretWithGenesisKey _ userSecret =
    (, userSecret) <$> fetchPrimaryKey userSecret

getKeyfilePath :: Args -> FilePath
getKeyfilePath = keyfilePath

updateUserSecretVSS
    :: (MonadIO m, MonadFail m) => Args -> UserSecret -> m UserSecret
updateUserSecretVSS _ = fillUserSecretVSS
#endif

fetchPrimaryKey :: MonadFail m => UserSecret -> m SecretKey
fetchPrimaryKey userSecret = case userSecret ^? usKeys . _head of
    Nothing -> fail $ "No secret keys are found in " ++ getUSPath userSecret
    Just sk -> return sk

fillUserSecretVSS :: (MonadIO m, MonadFail m) => UserSecret -> m UserSecret
fillUserSecretVSS userSecret = case userSecret ^. usVss of
    Just _  -> return userSecret
    Nothing -> do
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
                stakesDistr (CLI.flatDistr commonArgs) (CLI.bitcoinDistr commonArgs)
        , npTimeLord = timeLord
        , npJLFile = jlPath
        , npAttackTypes = maliciousEmulationAttacks
        , npAttackTargets = maliciousEmulationTargets
        , npPropagation = not (CLI.disablePropagation commonArgs)
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
    -> [SendActions BiP (RawRealMode ssc) -> RawRealMode ssc ()]
walletServe Args {..} =
    if enableWallet
    then [\sendActions -> walletServeWebFull sendActions walletDebug walletDbPath
        walletRebuildDb walletPort]
    else []

walletProd
    :: SscConstraint ssc
    => Args
    -> [SendActions BiP (ProductionMode ssc) -> ProductionMode ssc ()]
walletProd = map liftPlugin . walletServe
  where
    liftPlugin = \p sa -> lift . p $ hoistSendActions getNoStatsT lift sa

walletStats
    :: SscConstraint ssc
    => Args
    -> [SendActions BiP (StatsMode ssc) -> StatsMode ssc ()]
walletStats = map (liftPlugin) . walletServe
  where
    liftPlugin = \p sa -> do
        s <- getStatsMap
        lift . p $ hoistSendActions (runStatsT' s) lift sa
#else
walletProd, walletStats :: Args -> [a]
walletProd _ = []
walletStats _ = []
#endif

main :: IO ()
main = do
    args@Args{..} <- getNodeOptions
    bracketResources (baseParams "node" args) (action args)
