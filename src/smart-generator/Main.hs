{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Main where

import           Control.Concurrent.Async.Lifted (forConcurrently)
import           Control.Concurrent.STM.TVar     (modifyTVar', newTVarIO, readTVarIO)
import           Control.TimeWarp.Rpc            (NetworkAddress)
import           Control.TimeWarp.Timed          (Microsecond, for, fork_, ms, sec, wait)
import           Data.List                       ((!!))
import           Data.Maybe                      (fromMaybe)
import           Data.Time.Clock.POSIX           (getPOSIXTime)
import           Formatting                      (float, int, sformat, (%))
import           Options.Applicative             (execParser)
import           System.FilePath.Posix           ((</>))
import           System.Random.Shuffle           (shuffleM)
import           System.Wlog                     (logInfo)
import           Test.QuickCheck                 (arbitrary, generate)
import           Universum                       hiding (forConcurrently)

import qualified Pos.CLI                         as CLI
import           Pos.Constants                   (k, neighborsSendThreshold, slotDuration)
import           Pos.Crypto                      (KeyPair (..), hash)
import           Pos.DHT.Model                   (DHTNodeType (..), MonadDHT, dhtAddr,
                                                  discoverPeers, getKnownPeers)
import           Pos.DHT.Real                    (KademliaDHT (..), KademliaDHTInstance)
import           Pos.Genesis                     (genesisSecretKeys, genesisUtxo)
import           Pos.Launcher                    (BaseParams (..), LoggingParams (..),
                                                  NodeParams (..), bracketDHTInstance,
                                                  runNode, runProductionMode,
                                                  runTimeSlaveReal, stakesDistr)
import           Pos.Ssc.Class                   (SscConstraint, SscParams)
import           Pos.Ssc.GodTossing              (GtParams (..), SscGodTossing)
import           Pos.Ssc.NistBeacon              (SscNistBeacon)
import           Pos.Ssc.SscAlgo                 (SscAlgo (..))
import           Pos.State                       (isTxVerified)
import           Pos.Statistics                  (NoStatsT (..))
import           Pos.Types                       (Tx (..), TxWitness)
import           Pos.Util.JsonLog                ()
import           Pos.Wallet                      (submitTxRaw)
import           Pos.WorkMode                    (ProductionMode)

import           GenOptions                      (GenOptions (..), optsInfo)
import           TxAnalysis                      (checkWorker, createTxTimestamps,
                                                  registerSentTx)
import           TxGeneration                    (BambooPool, createBambooPool,
                                                  curBambooTx, initTransaction,
                                                  nextValidTx, resetBamboo)
import           Util


-- | Resend initTx with `slotDuration` period until it's verified
seedInitTx :: forall ssc . SscConstraint ssc
           => Double -> BambooPool -> (Tx, TxWitness) -> ProductionMode ssc ()
seedInitTx recipShare bp initTx = do
    na <- getPeers recipShare
    logInfo "Issuing seed transaction"
    submitTxRaw na initTx
    logInfo "Waiting for 1 slot before resending..."
    wait $ for slotDuration
    -- If next tx is present in utxo, then everything is all right
    tx <- liftIO $ curBambooTx bp 1
    isVer <- isTxVerified tx
    if isVer
        then pure ()
        else seedInitTx recipShare bp initTx

chooseSubset :: Double -> [a] -> [a]
chooseSubset share ls = take n ls
  where n = max 1 $ round $ share * fromIntegral (length ls)

getPeers :: (MonadDHT m, MonadIO m)
         => Double -> m [NetworkAddress]
getPeers share = do
    peers <- fmap dhtAddr <$> do
        ps <- getKnownPeers
        if length ps < neighborsSendThreshold
           then discoverPeers DHTFull
           else return ps
    liftIO $ chooseSubset share <$> shuffleM peers

runSmartGen :: forall ssc . SscConstraint ssc
            => KademliaDHTInstance -> NodeParams -> SscParams ssc -> GenOptions -> IO ()
runSmartGen inst np@NodeParams{..} sscnp opts@GenOptions{..} =
    runProductionMode inst np sscnp $ do
    let getPosixMs = round . (*1000) <$> liftIO getPOSIXTime
        initTx = initTransaction opts

    bambooPools <- forM goGenesisIdxs $ \(fromIntegral -> i) ->
                    liftIO $ createBambooPool
                      (genesisSecretKeys !! i)
                      (initTx i)

    txTimestamps <- liftIO createTxTimestamps

    -- | Run all the usual node workers in order to get
    -- access to blockchain
    fork_ $ runNode @ssc []

    let logsFilePrefix = fromMaybe "." (CLI.logPrefix goCommonArgs)
    -- | Run the special worker to check new blocks and
    -- fill tx verification times
    fork_ $ checkWorker txTimestamps logsFilePrefix

    logInfo "STARTING TXGEN"

    let forFold init ls act = foldM act init ls

    -- [CSL-220] Write MonadBaseControl instance for KademliaDHT
    -- Seeding init tx
    _ <- NoStatsT $ KademliaDHT $ forConcurrently goGenesisIdxs $
        \(fromIntegral -> i) -> unKademliaDHT $ getNoStatsT $
            seedInitTx goRecipientShare (bambooPools !! i) (initTx i)

    -- Start writing tps file
    liftIO $ writeFile (logsFilePrefix </> tpsCsvFile) tpsCsvHeader

    let phaseDurationMs = fromIntegral (k + goPropThreshold) * slotDuration
        roundDurationSec = fromIntegral (goRoundPeriodRate + 1) *
                           fromIntegral (phaseDurationMs `div` sec 1)

    void $ forFold (goInitTps, goTpsIncreaseStep) [1 .. goRoundNumber] $
        \(goTPS', increaseStep) (roundNum :: Int) -> do
        -- Start writing verifications file
        liftIO $ writeFile (logsFilePrefix </> verifyCsvFile roundNum) verifyCsvHeader


        let goTPS = goTPS' / fromIntegral (length bambooPools)
            tpsDelta = round $ 1000 / goTPS
            txNum = round $ roundDurationSec * goTPS

        logInfo $ sformat ("Round "%int%" from "%int%": TPS "%float)
            roundNum goRoundNumber goTPS

        realTxNum <- liftIO $ newTVarIO (0 :: Int)

        -- Make a pause between rounds
        wait $ for (round $ goRoundPause * fromIntegral (sec 1) :: Microsecond)

        beginT <- getPosixMs
        let startMeasurementsT =
                beginT + fromIntegral (phaseDurationMs `div` ms 1)

        let sendThread bambooPool = do
              logInfo $ sformat ("CURRENT TXNUM: "%int) txNum
              forM_ [0 .. txNum - 1] $ \(idx :: Int) -> do
                  preStartT <- getPosixMs
                  -- prevent periods longer than we expected
                  unless (preStartT - beginT > round (roundDurationSec * 1000)) $ do
                      startT <- getPosixMs

                      -- Get a random subset of neighbours to send tx
                      na <- getPeers goRecipientShare

                      eTx <- nextValidTx bambooPool goTPS goPropThreshold
                      case eTx of
                          Left parent -> do
                              logInfo $ sformat ("Transaction #"%int%" is not verified yet!") idx
                              logInfo "Resend the transaction parent again"
                              submitTxRaw na parent

                          Right (transaction, witness) -> do
                              let curTxId = hash transaction
                              logInfo $ sformat ("Sending transaction #"%int) idx
                              submitTxRaw na (transaction, witness)
                              when (startT >= startMeasurementsT) $ liftIO $ do
                                  liftIO $ atomically $ modifyTVar' realTxNum (+1)
                                  -- put timestamp to current txmap
                                  registerSentTx txTimestamps curTxId roundNum $ fromIntegral startT * 1000

                      endT <- getPosixMs
                      let runDelta = endT - startT
                      wait $ for $ ms (max 0 $ tpsDelta - runDelta)
              liftIO $ resetBamboo bambooPool

        -- [CSL-220] Write MonadBaseControl instance for KademliaDHT
        _ <- NoStatsT $ KademliaDHT $
            forConcurrently bambooPools (unKademliaDHT . getNoStatsT . sendThread)
        finishT <- getPosixMs

        realTxNumVal <- liftIO $ readTVarIO realTxNum

        let globalTime, realTPS :: Double
            globalTime = (fromIntegral (finishT - startMeasurementsT)) / 1000
            realTPS = (fromIntegral realTxNumVal) / globalTime
            (newTPS, newStep) = if realTPS >= goTPS' - 5
                                then (goTPS' + increaseStep, increaseStep)
                                else if realTPS >= goTPS' * 0.8
                                     then (goTPS', increaseStep)
                                     else (realTPS, increaseStep / 2)

        putText "----------------------------------------"
        putText $ "Sending transactions took (s): " <> show globalTime
        putText $ "So real tps was: " <> show realTPS

        -- We collect tables of really generated tps
        liftIO $ appendFile (logsFilePrefix </> tpsCsvFile) $
            tpsCsvFormat (globalTime, (goTPS, length bambooPools), realTPS)

        -- Wait for 1 phase (to get all the last sent transactions)
        logInfo "Pausing transaction spawning for 1 phase"
        wait $ for phaseDurationMs

        return (newTPS, newStep)

-----------------------------------------------------------------------------
-- Main
-----------------------------------------------------------------------------

main :: IO ()
main = do
    opts@GenOptions {..} <- execParser optsInfo

    KeyPair _ sk <- generate arbitrary
    vssKeyPair <- generate arbitrary
    let logParams =
            LoggingParams
            { lpRunnerTag     = "smart-gen"
            , lpHandlerPrefix = CLI.logPrefix goCommonArgs
            , lpConfigPath    = CLI.logConfig goCommonArgs
            }
        baseParams =
            BaseParams
            { bpLoggingParams      = logParams
            , bpPort               = 24962 + (fromIntegral $ minimum goGenesisIdxs)
            , bpDHTPeers           = CLI.dhtPeers goCommonArgs
            , bpDHTKeyOrType       = Right DHTFull
            , bpDHTExplicitInitial = CLI.dhtExplicitInitial goCommonArgs
            }

    bracketDHTInstance baseParams $ \inst -> do
        let timeSlaveParams =
                baseParams
                { bpLoggingParams = logParams { lpRunnerTag = "time-slave" }
                }

        systemStart <- runTimeSlaveReal inst timeSlaveParams

        let params =
                NodeParams
                { npDbPath        = Nothing
                , npDbPathM       = "zhogovo"
                , npRebuildDb     = False
                , npSystemStart   = systemStart
                , npSecretKey     = sk
                , npBaseParams    = baseParams
                , npCustomUtxo    = genesisUtxo $
                                        stakesDistr
                                        (CLI.flatDistr goCommonArgs)
                                        (CLI.bitcoinDistr goCommonArgs)
                , npTimeLord      = False
                , npJLFile        = goJLFile
                , npAttackTypes   = []
                , npAttackTargets = []
                , npPropagation   = not (CLI.disablePropagation goCommonArgs)
                }
            gtParams =
                GtParams
                { gtpRebuildDb  = False
                , gtpDbPath     = Nothing
                , gtpSscEnabled = False
                , gtpVssKeyPair = vssKeyPair
                }

        case CLI.sscAlgo goCommonArgs of
            GodTossingAlgo -> putText "Using MPC coin tossing" *>
                              runSmartGen @SscGodTossing inst params gtParams opts
            NistBeaconAlgo -> putText "Using NIST beacon" *>
                              runSmartGen @SscNistBeacon inst params () opts
