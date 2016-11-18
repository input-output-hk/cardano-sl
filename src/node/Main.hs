{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Applicative        (empty)
import           Control.Monad              (fail)
import           Data.Binary                (Binary, decode, encode)
import qualified Data.ByteString.Lazy       as LBS
import           Data.Default               (def)
import           Data.List                  ((!!))
import           Options.Applicative.Simple (simpleOptions)
import           System.Directory           (createDirectoryIfMissing)
import           System.FilePath            ((</>))
import           Universum

import           Pos.Constants              (RunningMode (..), runningMode)
import           Pos.Crypto                 (keyGen, vssKeyGen)
import           Pos.DHT                    (DHTNodeType (..), dhtNodeType)
import           Pos.Genesis                (StakeDistribution (..), genesisSecretKeys,
                                             genesisUtxo)
import           Pos.Launcher               (BaseParams (..), LoggingParams (..),
                                             NodeParams (..), RealModeRunner,
                                             bracketDHTInstance, runNodeReal,
                                             runNodeStats, runSupporterReal,
                                             runTimeLordReal, runTimeSlaveReal)
import           Pos.Ssc.GodTossing         (genesisVssKeyPairs)

import           NodeOptions                (Args (..), argsParser)
import           Pos.Ssc.GodTossing         (SscGodTossing)
import           Pos.Ssc.NistBeacon         (SscNistBeacon)
import           Pos.Ssc.SscAlgo            (SscAlgo (..))

getKey :: Binary key => Maybe key -> Maybe FilePath -> FilePath -> IO key -> IO key
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

realModeRunner :: Bool -> SscAlgo -> RealModeRunner
realModeRunner False GodTossingAlgo = runNodeReal @SscGodTossing
realModeRunner True GodTossingAlgo  = runNodeStats @SscGodTossing
realModeRunner False NistBeaconAlgo = runNodeReal @SscNistBeacon
realModeRunner True NistBeaconAlgo  = runNodeStats @SscNistBeacon

main :: IO ()
main = do
    (args, ()) <-
        simpleOptions
            "cardano-node"
            "PoS prototype node"
            "Use it!"
            argsParser
            empty
    bracketDHTInstance (baseParams "node" args) (action args)
  where
    action args@Args {..} inst = do
        case dhtKey of
            Just key -> do
                let type_ = dhtNodeType key
                if type_ ==
                   Just
                       (if supporterNode
                            then DHTSupporter
                            else DHTFull)
                    then return ()
                    else case type_ of
                             Just type_' ->
                                 fail $
                                 "Id of type " ++ (show type_') ++ " supplied"
                             _ -> fail "Id of unknown type supplied"
            _ -> return ()
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
                let currentParams = params args spendingSK vssSK systemStart
                putText $ "Running using " <> show sscAlgo
                realModeRunner enableStats sscAlgo inst currentParams
    getSystemStart inst args =
        case runningMode of
            Development ->
                if timeLord args
                    then runTimeLordReal (loggingParams "time-lord" args)
                    else runTimeSlaveReal inst (baseParams "time-slave" args)
            Production systemStart -> return systemStart
    loggingParams lpRunnerTag Args{..} =
        LoggingParams
        { lpHandlerPrefix = logsPrefix
        , lpConfigPath    = logConfig
        , ..
        }
    baseParams logger args@Args{..} =
        BaseParams
        { bpLoggingParams = loggingParams logger args
        , bpPort = port
        , bpDHTPeers = dhtPeers
        , bpDHTKeyOrType =
              if supporterNode
                  then maybe (Right DHTSupporter) Left dhtKey
                  else maybe (Right DHTFull) Left dhtKey
        , bpDHTExplicitInitial = dhtExplicitInitial
        }
    params args@Args {..} spendingSK vssSK systemStart =
        NodeParams
        { npDbPath = Just dbPath
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
        }
    stakesDistr Args {..} =
        case (flatDistr, bitcoinDistr) of
            (Nothing, Nothing) -> def
            (Just _, Just _) ->
                panic "flat-distr and bitcoin distr are conflicting options"
            (Just (nodes, coins), Nothing) ->
                FlatStakes (fromIntegral nodes) (fromIntegral coins)
            (Nothing, Just (nodes, coins)) ->
                BitcoinStakes (fromIntegral nodes) (fromIntegral coins)
