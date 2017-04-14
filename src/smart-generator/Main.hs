{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import           Control.Concurrent.STM.TVar (readTVarIO)
import           Control.Monad.Trans.Class   (MonadTrans)
import           Data.Maybe                  (fromMaybe)
import qualified Data.Set                  as Set (fromList)
import           Data.Time.Clock.POSIX       (getPOSIXTime)
import           Data.Time.Units             (Microsecond, convertUnit)
import           Formatting                  (float, int, sformat, (%))
import           Mockable                    (Production, delay, forConcurrently, fork)
import           Options.Applicative         (execParser)
import           Serokell.Util               (ms, sec)
import           System.FilePath             ((</>))
import           System.Random.Shuffle       (shuffleM)
import           System.Wlog                 (logInfo)
import           Test.QuickCheck             (arbitrary, generate)
import           Universum

import qualified Pos.CLI                     as CLI
import           Pos.Communication           (ActionSpec (..), SendActions,
                                              convertSendActions, sendTxOuts, submitTxRaw,
                                              wrapSendActions, PeerId, NodeId)
import           Pos.Constants               (genesisN, genesisSlotDuration,
                                              neighborsSendThreshold, slotSecurityParam)
import           Pos.Crypto                  (hash)
import           Pos.Genesis                 (genesisUtxo)
import           Pos.Launcher                (BaseParams (..), LoggingParams (..),
                                              NodeParams (..), RealModeResources (..),
                                              bracketResources, initLrc, runNode',
                                              runProductionMode, stakesDistr,
                                              hoistResources)
import           Pos.Ssc.Class               (SscConstraint, SscParams)
import           Pos.Ssc.GodTossing          (GtParams (..), SscGodTossing)
import           Pos.Ssc.NistBeacon          (SscNistBeacon)
import           Pos.Ssc.SscAlgo             (SscAlgo (..))
import           Pos.Txp                     (TxAux)
import           Pos.Update.Params           (UpdateParams (..))
import           Pos.Util.JsonLog            ()
import           Pos.Util.UserSecret         (simpleUserSecret)
import           Pos.Worker                  (allWorkers)
import           Pos.WorkMode                (ProductionMode, RawRealMode)

import           GenOptions                  (GenOptions (..), optsInfo)
import           TxAnalysis                  (checkWorker, createTxTimestamps,
                                              registerSentTx)
import           TxGeneration                (BambooPool, createBambooPool, curBambooTx,
                                              initTransaction, isTxVerified, nextValidTx,
                                              resetBamboo)
import           Util
import qualified Network.Transport.TCP       as TCP (TCPAddr (..))


-- | Resend initTx with 'slotDuration' period until it's verified
seedInitTx :: forall ssc . SscConstraint ssc
           => RealModeResources (ProductionMode ssc)
           -> SendActions (ProductionMode ssc)
           -> Double
           -> BambooPool
           -> TxAux
           -> ProductionMode ssc ()
seedInitTx res sendActions recipShare bp initTx = do
    na <- getPeersShare res recipShare
    logInfo "Issuing seed transaction"
    submitTxRaw sendActions na initTx
    logInfo "Waiting for 1 slot before resending..."
    delay genesisSlotDuration
    -- If next tx is present in utxo, then everything is all right
    tx <- liftIO $ curBambooTx bp 1
    isVer <- isTxVerified $ view _1 tx
    if isVer
        then pure ()
        else seedInitTx res sendActions recipShare bp initTx

chooseSubset :: Double -> [a] -> [a]
chooseSubset share ls = take n ls
  where n = max 1 $ round $ share * fromIntegral (length ls)

getPeersShare
    :: (MonadIO m)
    => RealModeResources m
    -> Double
    -> m [NodeId]
getPeersShare res share = do
    peers <- do
        ps <- fmap toList (rmGetPeers res)
        if length ps < neighborsSendThreshold
           then fmap toList (rmFindPeers res)
           else return ps
    liftIO $ chooseSubset share <$> shuffleM peers

runSmartGen :: forall ssc . SscConstraint ssc
            => PeerId -> RealModeResources (ProductionMode ssc) -> NodeParams -> SscParams ssc -> GenOptions -> Production ()
runSmartGen peerId res np@NodeParams{..} sscnp opts@GenOptions{..} =
  runProductionMode peerId res np sscnp $ (,sendTxOuts <> wOuts) . ActionSpec $ \vI sendActions -> do
    initLrc
    let getPosixMs = round . (*1000) <$> liftIO getPOSIXTime
        initTx = initTransaction opts

    bambooPools <- forM goGenesisIdxs $ \(fromIntegral -> i) ->
        liftIO $ createBambooPool goMOfNParams i $ initTx i

    txTimestamps <- liftIO createTxTimestamps

    let ActionSpec nodeAction = runNode' @ssc res workers'

    -- | Run all the usual node workers in order to get
    -- access to blockchain
    void $ fork $ nodeAction vI sendActions

    let logsFilePrefix = fromMaybe "." (CLI.logPrefix goCommonArgs)
    -- | Run the special worker to check new blocks and
    -- fill tx verification times
    void $ fork $ checkWorker txTimestamps logsFilePrefix

    logInfo "STARTING TXGEN"

    let forFold init ls act = foldM act init ls
        sA = convertSendActions vI $ wrapSendActions sendActions

    -- [CSL-220] Write MonadBaseControl instance for KademliaDHT
    -- Seeding init tx
    void $ forConcurrently (zip bambooPools goGenesisIdxs) $ \(pool, fromIntegral -> idx) ->
         seedInitTx res sA goRecipientShare pool (initTx idx)

    -- Start writing tps file
    writeFile (logsFilePrefix </> tpsCsvFile) tpsCsvHeader

    let phaseDurationMs :: Microsecond
        phaseDurationMs =
            fromIntegral (slotSecurityParam + goPropThreshold) *
            convertUnit genesisSlotDuration
        roundDurationSec =
            fromIntegral (goRoundPeriodRate + 1) *
            fromIntegral (phaseDurationMs `div` sec 1)

    void $ forFold (goInitTps, goTpsIncreaseStep) [1 .. goRoundNumber] $
      \(goTPS', increaseStep) (roundNum :: Int) -> do
      -- Start writing verifications file
      writeFile (logsFilePrefix </> verifyCsvFile roundNum) verifyCsvHeader


      let goTPS = goTPS' / fromIntegral (length bambooPools)
          tpsDelta = round $ 1000 / goTPS
          txNum = round $ roundDurationSec * goTPS

      logInfo $ sformat ("Round "%int%" from "%int%": TPS "%float)
          roundNum goRoundNumber goTPS

      realTxNum <- newTVarIO (0 :: Int)

      -- Make a pause between rounds
      delay (round $ goRoundPause * fromIntegral (sec 1) :: Microsecond)

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
                    na <- getPeersShare res goRecipientShare

                    eTx <- nextValidTx bambooPool goTPS goPropThreshold
                    case eTx of
                        Left parent -> do
                            logInfo $ sformat ("Transaction #"%int%" is not verified yet!") idx
                            logInfo "Resend the transaction parent again"
                            submitTxRaw sA na parent

                        Right (transaction, witness, distr) -> do
                            let curTxId = hash transaction
                            logInfo $ sformat ("Sending transaction #"%int) idx
                            submitTxRaw sA na (transaction, witness, distr)
                            when (startT >= startMeasurementsT) $ liftIO $ do
                                atomically $ modifyTVar' realTxNum (+1)
                                -- put timestamp to current txmap
                                registerSentTx txTimestamps curTxId roundNum $
                                    fromIntegral startT * 1000

                    endT <- getPosixMs
                    let runDelta = endT - startT
                    delay $ ms (max 0 $ tpsDelta - runDelta)
            liftIO $ resetBamboo bambooPool

      -- [CSL-220] Write MonadBaseControl instance for KademliaDHT
      _ <- forConcurrently bambooPools sendThread
      finishT <- getPosixMs

      realTxNumVal <- liftIO $ readTVarIO realTxNum

      let globalTime, realTPS :: Double
          globalTime = (fromIntegral (finishT - startMeasurementsT)) / 1000
          realTPS = (fromIntegral realTxNumVal) / globalTime
          (newTPS, newStep)
              | realTPS >= goTPS' - 5 = (goTPS' + increaseStep, increaseStep)
              | realTPS >= goTPS' * 0.8 = (goTPS', increaseStep)
              | otherwise = (realTPS, increaseStep / 2)

      putText "----------------------------------------"
      putText $ "Sending transactions took (s): " <> show globalTime
      putText $ "So real tps was: " <> show realTPS

      -- We collect tables of really generated tps
      appendFile (logsFilePrefix </> tpsCsvFile) $
          tpsCsvFormat (globalTime, (goTPS, length bambooPools), realTPS)

      -- Wait for 1 phase (to get all the last sent transactions)
      logInfo "Pausing transaction spawning for 1 phase"
      delay phaseDurationMs

      return (newTPS, newStep)
  where
    (workers', wOuts) = allWorkers (rmGetPeers res)

-----------------------------------------------------------------------------
-- Main
-----------------------------------------------------------------------------

main :: IO ()
main = do
    opts@GenOptions {..} <- execParser optsInfo

    -- Check correctness of --m-of-n param
    case goMOfNParams of
        Nothing     -> return ()
        Just (m, n) -> when (m > n || n > genesisN) $ error "Invalid `--m-of-n` value"

    sk <- generate arbitrary
    vssKeyPair <- generate arbitrary
    --filePeers <- maybe (return []) CLI.readPeersFile
    --                 (CLI.dhtPeersFile goCommonArgs)
    let allPeers = goPeers -- ++ filePeers
    let logParams =
            LoggingParams
            { lpRunnerTag     = "smart-gen"
            , lpHandlerPrefix = CLI.logPrefix goCommonArgs
            , lpConfigPath    = CLI.logConfig goCommonArgs
            , lpEkgPort       = Nothing
            }
        baseParams =
            BaseParams
            { bpLoggingParams = logParams
            }

    bracketResources baseParams TCP.Unaddressable (Set.fromList allPeers) $ \res -> do
        let trans :: forall m t . MonadTrans m => Production t -> (forall ssc . m (RawRealMode ssc) t)
            trans = lift . lift . lift . lift . lift . lift . lift . lift . lift . lift
            res' :: forall ssc . RealModeResources (ProductionMode ssc)
            res' = hoistResources trans res

        let peerId = CLI.peerId goCommonArgs
        let systemStart = CLI.sysStart goCommonArgs

        let params =
                NodeParams
                { npDbPathM       = "rocks-smartwallet"
                , npRebuildDb     = True
                , npSystemStart   = systemStart
                , npSecretKey     = sk
                , npUserSecret    = simpleUserSecret sk "smartgen-secret.sk"
                , npBaseParams    = baseParams
                , npCustomUtxo    = genesisUtxo $
                                        stakesDistr
                                        (CLI.flatDistr goCommonArgs)
                                        (CLI.bitcoinDistr goCommonArgs)
                                        (CLI.richPoorDistr goCommonArgs)
                                        (CLI.expDistr goCommonArgs)
                , npJLFile        = goJLFile
                , npAttackTypes   = []
                , npAttackTargets = []
                , npPropagation   = not (CLI.disablePropagation goCommonArgs)
                , npReportServers = []
                , npUpdateParams = UpdateParams
                    { upUpdatePath    = "update.exe"
                    , upUpdateWithPkg = True
                    , upUpdateServers = []
                    }
                }
            gtParams =
                GtParams
                { gtpSscEnabled = False
                , gtpVssKeyPair = vssKeyPair
                }

        case CLI.sscAlgo goCommonArgs of
            GodTossingAlgo -> putText "Using MPC coin tossing" *>
                              runSmartGen @SscGodTossing peerId res' params gtParams opts
            NistBeaconAlgo -> putText "Using NIST beacon" *>
                              runSmartGen @SscNistBeacon peerId res' params () opts
