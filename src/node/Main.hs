{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Main where

import           Control.Monad        (fail)
import qualified Data.ByteString.Lazy as LBS
import           Data.List            ((!!))
import           System.Directory     (createDirectoryIfMissing)
import           System.FilePath      ((</>))
import           System.Wlog          (LoggerName)
import           Universum

import           Pos.Binary           (Bi, decode, encode)
import           Pos.Constants        (RunningMode (..), runningMode)
import           Pos.Crypto           (SecretKey, VssKeyPair, keyGen, vssKeyGen)
import           Pos.DHT              (DHTKey, DHTNodeType (..), dhtNodeType)
import           Pos.DHT.Real         (KademliaDHTInstance)
import           Pos.Genesis          (genesisSecretKeys, genesisUtxo)
import           Pos.Launcher         (BaseParams (..), LoggingParams (..),
                                       NodeParams (..), bracketDHTInstance,
                                       runNodeProduction, runNodeStats, runSupporterReal,
                                       runTimeLordReal, runTimeSlaveReal, stakesDistr)
import           Pos.Ssc.GodTossing   (genesisVssKeyPairs)
import           Pos.Ssc.GodTossing   (GtParams (..), SscGodTossing)
import           Pos.Ssc.NistBeacon   (SscNistBeacon)
import           Pos.Ssc.SscAlgo      (SscAlgo (..))
import           Pos.Types            (Timestamp)
#ifdef WITH_WEB
import           Pos.Ssc.Class        (SscConstraint)
import           Pos.Web              (serveWebBase, serveWebGT)
import           Pos.WorkMode         (WorkMode)
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

getSystemStart :: KademliaDHTInstance -> Args -> IO Timestamp
getSystemStart inst args =
    case runningMode of
        Development ->
            if timeLord args
                then runTimeLordReal (loggingParams "time-lord" args)
                else runTimeSlaveReal inst (baseParams "time-slave" args)
        Production systemStart -> return systemStart

loggingParams :: LoggerName -> Args -> LoggingParams
loggingParams tag Args{..} =
    LoggingParams
    { lpHandlerPrefix = logsPrefix
    , lpConfigPath    = logConfig
    , lpRunnerTag = tag
    }

baseParams :: LoggerName -> Args -> BaseParams
baseParams loggingTag args@Args {..} =
    BaseParams
    { bpLoggingParams = loggingParams loggingTag args
    , bpPort = port
    , bpDHTPeers = dhtPeers
    , bpDHTKeyOrType = dhtKeyOrType
    , bpDHTExplicitInitial = dhtExplicitInitial
    }
  where
    dhtKeyOrType
        | supporterNode = maybe (Right DHTSupporter) Left dhtKey
        | otherwise = maybe (Right DHTFull) Left dhtKey

checkDhtKey :: Bool -> Maybe DHTKey -> IO ()
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

action :: Args -> KademliaDHTInstance -> IO ()
action args@Args {..} inst = do
    checkDhtKey supporterNode dhtKey
    if supporterNode
        then runSupporterReal inst (baseParams "supporter" args)
        else do
            spendingSK <-
                getKey
                    ((genesisSecretKeys !!) <$> spendingGenesisI)
                    spendingSecretPath
                    "spending"
                    (snd <$> keyGen)
            vssSK <-
                getKey
                    ((genesisVssKeyPairs !!) <$> vssGenesisI)
                    vssSecretPath
                    "vss.keypair"
                    vssKeyGen
            systemStart <- getSystemStart inst args
            let currentParams = nodeParams args spendingSK systemStart
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
            putText $ "Running using " <> show sscAlgo
            case (enableStats, sscAlgo) of
                (True, GodTossingAlgo) ->
                    runNodeStats @SscGodTossing inst currentPluginsGT currentParams gtParams
                (True, NistBeaconAlgo) ->
                    runNodeStats @SscNistBeacon inst currentPlugins currentParams ()
                (False, GodTossingAlgo) ->
                    runNodeProduction @SscGodTossing inst currentPluginsGT currentParams gtParams
                (False, NistBeaconAlgo) ->
                    runNodeProduction @SscNistBeacon inst currentPlugins currentParams ()

nodeParams :: Args -> SecretKey -> Timestamp -> NodeParams
nodeParams args@Args {..} spendingSK systemStart =
    NodeParams
    { npDbPath = if memoryMode then Nothing
                 else Just dbPath
    , npDbPathM = dbPath
    , npRebuildDb = rebuildDB
    , npSecretKey = spendingSK
    , npSystemStart = systemStart
    , npBaseParams = baseParams "node" args
    , npCustomUtxo =
            Just . genesisUtxo $
            stakesDistr flatDistr bitcoinDistr
    , npTimeLord = timeLord
    , npJLFile = jlPath
    , npMalicious = maliciousEmulation
    , npMalicious' = maliciousEmulation'
    }

gtSscParams :: Args -> VssKeyPair -> GtParams
gtSscParams Args {..} vssSK =
    GtParams
    {
      gtpRebuildDb  = rebuildDB
    , gtpDbPath     = if memoryMode then Nothing
                      else Just dbPath
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

main :: IO ()
main = do
    args <- getNodeOptions
    bracketDHTInstance (baseParams "node" args) (action args)
