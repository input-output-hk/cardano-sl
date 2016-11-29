{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Main where

import           Control.Monad        (fail)
import           Data.Binary          (Binary, decode, encode)
import qualified Data.ByteString.Lazy as LBS
import           Data.Default         (def)
import           Data.List            ((!!))
import           System.Directory     (createDirectoryIfMissing)
import           System.FilePath      ((</>))
import           System.Wlog          (LoggerName)
import           Universum

import           Pos.Constants        (RunningMode (..), runningMode)
import           Pos.Crypto           (SecretKey, VssKeyPair, keyGen, vssKeyGen)
import           Pos.DHT              (DHTKey, DHTNodeType (..), dhtNodeType)
import           Pos.DHT.Real         (KademliaDHTInstance)
import           Pos.Genesis          (StakeDistribution (..), genesisSecretKeys,
                                       genesisUtxo)
import           Pos.Launcher         (BaseParams (..), LoggingParams (..),
                                       NodeParams (..), NodeRunner, bracketDHTInstance,
                                       runNodeProduction, runNodeStats, runSupporterReal,
                                       runTimeLordReal, runTimeSlaveReal)
import           Pos.Ssc.GodTossing   (genesisVssKeyPairs)
import           Pos.Ssc.GodTossing   (SscGodTossing)
import           Pos.Ssc.NistBeacon   (SscNistBeacon)
import           Pos.Ssc.SscAlgo      (SscAlgo (..))
import           Pos.Types            (Timestamp)

import           NodeOptions          (Args (..), getNodeOptions)

getKey
    :: Binary key
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

decode' :: Binary key => FilePath -> IO key
decode' fpath = either fail' return . decode =<< LBS.readFile fpath
  where
    fail' e = fail $ "Error reading key from " ++ fpath ++ ": " ++ e

nodeRunner :: Bool -> SscAlgo -> NodeRunner
nodeRunner False GodTossingAlgo = runNodeProduction @SscGodTossing
nodeRunner True GodTossingAlgo  = runNodeStats @SscGodTossing
nodeRunner False NistBeaconAlgo = runNodeProduction @SscNistBeacon
nodeRunner True NistBeaconAlgo  = runNodeStats @SscNistBeacon

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

stakesDistr :: Args -> StakeDistribution
stakesDistr Args {..} =
    case (flatDistr, bitcoinDistr) of
        (Nothing, Nothing) -> def
        (Just _, Just _) ->
            panic "flat-distr and bitcoin distr are conflicting options"
        (Just (nodes, coins), Nothing) ->
            FlatStakes (fromIntegral nodes) (fromIntegral coins)
        (Nothing, Just (nodes, coins)) ->
            BitcoinStakes (fromIntegral nodes) (fromIntegral coins)

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
            let currentParams = nodeParams args spendingSK vssSK systemStart
            putText $ "Running using " <> show sscAlgo
            nodeRunner enableStats sscAlgo inst currentParams

nodeParams :: Args -> SecretKey -> VssKeyPair -> Timestamp -> NodeParams
nodeParams args@Args {..} spendingSK vssSK systemStart =
    NodeParams
    { npDbPath = if memoryMode then Nothing
                    else Just dbPath
    , npRebuildDb = rebuildDB
    , npSecretKey = spendingSK
    , npSystemStart = systemStart
    , npVssKeyPair = vssSK
    , npBaseParams = baseParams "node" args
    , npCustomUtxo =
            Just . genesisUtxo $
            stakesDistr args
    , npTimeLord = timeLord
    , npJLFile = jlPath
    , npSscEnabled = True
    }

main :: IO ()
main = do
    args <- getNodeOptions
    bracketDHTInstance (baseParams "node" args) (action args)
