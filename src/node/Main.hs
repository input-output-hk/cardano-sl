{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.ByteString.Lazy as LBS
import           Data.List            ((!!))
import           Node                 (SendActions, hoistSendActions)
import           Mockable             (Production)
import           System.Directory     (createDirectoryIfMissing)
import           System.FilePath      ((</>))
import           System.Wlog          (LoggerName)
import           Universum

import           Pos.Binary           (Bi, decode, encode)
import qualified Pos.CLI              as CLI
import           Pos.Communication     (BiP)
import           Pos.Constants        (RunningMode (..), runningMode)
import           Pos.Crypto           (VssKeyPair, vssKeyGen)
import           Pos.Genesis          (genesisSecretKeys, genesisUtxo)
import           Pos.Launcher         (BaseParams (..), LoggingParams (..),
                                       NodeParams (..), RealModeResources,
                                       bracketResources, runNodeProduction, runNodeStats,
                                       runTimeLordReal, runTimeSlaveReal, stakesDistr)
import           Pos.DHT.Model     (DHTKey, DHTNodeType (..), dhtNodeType)
import           Pos.Ssc.GodTossing   (genesisVssKeyPairs)
import           Pos.Ssc.GodTossing   (GtParams (..), SscGodTossing)
import           Pos.Ssc.NistBeacon   (SscNistBeacon)
import           Pos.Ssc.SscAlgo      (SscAlgo (..))
import           Pos.Statistics       (getNoStatsT, runStatsT', getStatsMap)
import           Pos.Types            (Timestamp)
#ifdef WITH_WEB
import           Pos.Ssc.Class        (SscConstraint)
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

getSystemStart :: RealModeResources -> Args -> Production Timestamp
getSystemStart inst args  =
    case runningMode of
        Development ->
            if timeLord args
                then runTimeLordReal (loggingParams "time-lord" args)
                else runTimeSlaveReal inst (baseParams "time-slave" args)
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
            vssSK <-
                liftIO $ getKey
                    ((genesisVssKeyPairs !!) <$> vssGenesisI)
                    vssSecretPath
                    "vss.keypair"
                    vssKeyGen
            systemStart <- getSystemStart res args
            let currentParams = nodeParams args systemStart
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

nodeParams :: Args -> Timestamp -> NodeParams
nodeParams args@Args {..} systemStart =
    NodeParams
    { npDbPathM = dbPath
    , npRebuildDb = rebuildDB
    , npSecretKey = (genesisSecretKeys !!) <$> spendingGenesisI
    , npKeyfilePath = maybe keyfilePath (\i -> "node-" ++ show i ++ "." ++ keyfilePath) spendingGenesisI
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
walletProd _ _ = []
walletStats _ _ = []
#endif

main :: IO ()
main = do
    args@Args{..} <- getNodeOptions
    bracketResources (baseParams "node" args) (action args)
