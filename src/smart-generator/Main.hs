{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Lens           ((^.))
import           Control.Monad          (when)
import           Control.TimeWarp.Rpc   (NetworkAddress)
import           Control.TimeWarp.Timed (for, fork_, ms, repeatForever, sec, wait)
import           Data.Array.IO          (IOArray)
import           Data.Array.MArray      (MArray (..), newListArray, readArray, writeArray)
import           Data.Default           (def)
import qualified Data.HashMap.Strict    as M
import           Data.IORef             (IORef, modifyIORef', newIORef, readIORef,
                                         writeIORef)
import           Data.List              (head, intersect, tail, (!!))
import           Data.Text.IO           (appendFile, writeFile)
import           Data.Time.Clock.POSIX  (getPOSIXTime)
import           Formatting             (build, fixed, float, int, sformat, (%))
import           Options.Applicative    (execParser)
import           System.Wlog            (logInfo, logWarning)
import           Test.QuickCheck        (arbitrary, generate)
import           Universum              hiding (head)

import           Pos.Communication      (allListeners)
import           Pos.Constants          (k, slotDuration)
import           Pos.Crypto             (KeyPair (..), SecretKey, hash, sign, unsafeHash)
import           Pos.DHT                (DHTNodeType (..), ListenerDHT, dhtAddr,
                                         discoverPeers, mapListenerDHT)
import           Pos.DHT.Real           (KademliaDHTInstance)
import           Pos.Genesis            (StakeDistribution (..), genesisAddresses,
                                         genesisSecretKeys, genesisUtxo)
import           Pos.Launcher           (BaseParams (..), LoggingParams (..),
                                         NodeParams (..), addDevListeners,
                                         bracketDHTInstance, runNode, runRealMode,
                                         runTimeSlaveReal, submitTxRaw)
import           Pos.Slotting           (getCurrentSlot, getSlotStart)
import           Pos.Ssc.Class          (SscConstraint)
import           Pos.Ssc.GodTossing     (SscGodTossing)
import           Pos.Ssc.NistBeacon     (SscNistBeacon)
import           Pos.Ssc.SscAlgo        (SscAlgo (..))
import           Pos.State              (getBlockByDepth, isTxVerified)
import           Pos.Statistics         (getNoStatsT)
import           Pos.Types              (Address, SlotId (..), Tx (..), TxId, TxIn (..),
                                         TxOut (..), blockSlot, blockTxs, txF)
import           Pos.Util.JsonLog       ()
import           Pos.WorkMode           (ProductionMode, RealMode, WorkMode)

import           GenOptions             (GenOptions (..), optsInfo)

----------------------------------------------------------------------------------------------------
-- Transactions generation
----------------------------------------------------------------------------------------------------

txChain :: Int -> [Tx]
txChain i = genChain (genesisSecretKeys !! i) addr (unsafeHash addr) 0
  where addr = genesisAddresses !! i

genChain :: SecretKey -> Address -> TxId -> Word32 -> [Tx]
genChain secretKey addr txInHash txInIndex =
    let txOutValue = 1
        txOutputs = [TxOut { txOutAddress = addr, ..}]
        txInputs = [TxIn { txInSig = sign secretKey (txInHash, txInIndex, txOutputs), .. }]
        resultTransaction = Tx {..}
    in resultTransaction : genChain secretKey addr (hash resultTransaction) 0

initTransaction :: GenOptions -> Tx
initTransaction GenOptions {..} =
    let maxTps = round $ goInitTps + goTpsIncreaseStep * fromIntegral goRoundNumber
        n' = maxTps * (k + goPropThreshold) * fromIntegral (slotDuration `div` sec 1)
        n = min n' goInitBalance
        i = fromIntegral goGenesisIdx
        txOutAddress = genesisAddresses !! i
        secretKey = genesisSecretKeys !! i
        txOutValue = 1
        txOutputs = replicate n (TxOut {..})
        txInHash = unsafeHash txOutAddress
        txInIndex = 0
        txInputs = [TxIn { txInSig = sign secretKey (txInHash, txInIndex, txOutputs), .. }]
    in Tx {..}

data BambooPool = BambooPool
    { bpChains :: IOArray Int [Tx]
    , bpCurIdx :: IORef Int
    }

createBambooPool :: SecretKey -> Address -> Tx -> IO BambooPool
createBambooPool sk addr tx = BambooPool <$> newListArray (0, outputsN - 1) bamboos <*> newIORef 0
    where outputsN = length $ txOutputs tx
          bamboos = --map (tx :) $
                    map (genChain sk addr (hash tx) . fromIntegral) [0 .. outputsN - 1]

shiftTx :: BambooPool -> IO ()
shiftTx BambooPool {..} = do
    idx <- readIORef bpCurIdx
    chain <- readArray bpChains idx
    writeArray bpChains idx $ tail chain

nextBamboo :: BambooPool -> IO ()
nextBamboo BambooPool {..} = do
    lastChainIdx <- snd <$> getBounds bpChains
    modifyIORef' bpCurIdx $ \idx ->
        (idx + 1) `mod` (lastChainIdx + 1)

peekTx :: BambooPool -> IO Tx
peekTx BambooPool {..} =
    readIORef bpCurIdx >>=
    fmap head . readArray bpChains

nextValidTx :: WorkMode ssc m => BambooPool -> Int -> m Tx
nextValidTx bp tpsDelta = do
    tx <- liftIO $ peekTx bp
    isVer <- isTxVerified tx
    if isVer
        then liftIO $ do
        shiftTx bp
        nextBamboo bp
        return tx
        else do
        logInfo $ sformat ("Transaction "%txF%"is not verified yet!") tx
        liftIO $ nextBamboo bp
        wait $ for $ ms tpsDelta
        nextValidTx bp tpsDelta

-----------------------------------------------------------------------------
-- Transaction analysis
-----------------------------------------------------------------------------

type TxTimeMap = M.HashMap TxId Word64

data TxTimestamps = TxTimestamps
    { sentTimes   :: IORef TxTimeMap
    , verifyTimes :: IORef TxTimeMap
    , lastSlot    :: IORef SlotId
    }

createTxTimestamps :: IO TxTimestamps
createTxTimestamps = TxTimestamps
                     <$> newIORef M.empty
                     <*> newIORef M.empty
                     <*> newIORef (SlotId 0 0)

registerSentTx :: TxTimestamps -> TxId -> Word64 -> IO ()
registerSentTx TxTimestamps{..} id = modifyIORef' sentTimes . M.insert id

registerVerifiedTx :: TxTimestamps -> TxId -> Word64 -> IO ()
registerVerifiedTx TxTimestamps{..} id = modifyIORef' verifyTimes . M.insert id

dumpTxTable :: TxTimestamps -> IO [(TxId, Word64, Word64)]
dumpTxTable TxTimestamps {..} = M.foldlWithKey' foo []
                                <$> (M.intersectionWith (,)
                                     <$> readIORef sentTimes
                                     <*> readIORef verifyTimes)
  where foo ls id (sent, verified) = (id, sent, verified) : ls

checkTxsInLastBlock :: forall ssc . SscConstraint ssc
                    => TxTimestamps -> ProductionMode ssc ()
checkTxsInLastBlock txts@TxTimestamps {..} = do
    mBlock <- getBlockByDepth k
    case mBlock of
        Nothing -> pure ()
        Just (Left _) -> pure ()
        Just (Right block) -> do
            st <- liftIO $ readIORef sentTimes
            vt <- liftIO $ readIORef verifyTimes
            ls <- liftIO $ readIORef lastSlot
            let curSlot = block^.blockSlot
            when (ls < curSlot) $ do
                let toCheck = M.keys $ M.difference st vt
                    txsMerkle = block^.blockTxs
                    txIds = map hash $ toList txsMerkle
                    verified = toCheck `intersect` txIds
                -- We don't know exact time when checked block has been created/adopted,
                -- but we do know that it was not at `k` depth a slot ago,
                -- so we just take a beginning of current slot
                slStart <- getSlotStart =<< getCurrentSlot
                forM_ verified $ \id ->
                    liftIO $ registerVerifiedTx txts id $ fromIntegral slStart
                liftIO $ writeIORef lastSlot curSlot

checkWorker :: forall ssc . SscConstraint ssc
            => TxTimestamps -> ProductionMode ssc ()
checkWorker txts = repeatForever slotDuration onError $
                   checkTxsInLastBlock txts
  where onError e = slotDuration <$
                    logWarning (sformat ("Error occured in checkWorker: " %build) e)

-----------------------------------------------------------------------------
-- Launcher helper
-----------------------------------------------------------------------------

realListeners :: SscConstraint ssc => NodeParams -> [ListenerDHT (RealMode ssc)]
realListeners params = addDevListeners params noStatsListeners
  where noStatsListeners = map (mapListenerDHT getNoStatsT) allListeners

-- | Resend initTx with `slotDuration` period until it's verified
seedInitTx :: forall ssc . SscConstraint ssc
           => BambooPool -> Tx -> [NetworkAddress] -> ProductionMode ssc ()
seedInitTx bp initTx na = do
    logInfo "Issuing seed transaction"
    submitTxRaw na initTx
    logInfo "Waiting for 1 slot before resending..."
    wait $ for slotDuration
    -- If next tx is present in utxo, then everything is all right
    tx <- liftIO $ peekTx bp
    isVer <- isTxVerified tx
    if isVer
        then pure ()
        else seedInitTx bp initTx na

runSmartGen :: forall ssc . SscConstraint ssc
            => KademliaDHTInstance -> NodeParams -> GenOptions -> IO ()
runSmartGen inst np@NodeParams{..} opts@GenOptions{..} =
    runRealMode inst np (realListeners @ssc np) $ getNoStatsT $ do
    let i = fromIntegral goGenesisIdx
        getPosixMs = round . (*1000) <$> liftIO getPOSIXTime
        initTx = initTransaction opts

    bambooPool <- liftIO $ createBambooPool
                  (genesisSecretKeys !! i)
                  (genesisAddresses !! i)
                  initTx

    txTimestamps <- liftIO createTxTimestamps

    -- | Run all the usual node workers in order to get
    -- access to blockchain
    fork_ $ runNode @ssc

    -- | Run the special worker to check new blocks and
    -- fill tx verification times
    fork_ $ checkWorker txTimestamps

    logInfo "STARTING TXGEN"
    peers <- discoverPeers DHTFull

    let na' = dhtAddr <$> peers
        na = if goSingleRecipient
             then take 1 na'
             else na'
        forFold init ls act = foldM act init ls

    -- Seeding init tx
    seedInitTx bambooPool initTx na

    -- Start writing data files
    liftIO $ do
        writeFile tpsCsvFile tpsCsvHeader
        writeFile verifyCsvFile verifyCsvHeader

    _ <- forFold (goInitTps, goTpsIncreaseStep) [1 .. goRoundNumber] $
        \(goTPS, increaseStep) (roundNum :: Int) -> do
        logInfo $ sformat ("Round "%int%" from "%int%": TPS "%float)
            roundNum goRoundNumber goTPS

        let tpsDelta = round $ 1000 / goTPS
            txNum = round $ goRoundDuration * goTPS

        realTxNum <- liftIO $ newIORef (0 :: Int)

        beginT <- getPosixMs
        forM_ [0 .. txNum - 1] $ \(idx :: Int) -> do
            preStartT <- getPosixMs
            logInfo $ sformat ("CURRENT TXNUM: "%int) txNum
            -- prevent periods longer than we expected
            unless (preStartT - beginT > round (goRoundDuration * 1000)) $ do
                transaction <- nextValidTx bambooPool tpsDelta
                let curTxId = hash transaction

                startT <- getPosixMs
                logInfo $ sformat ("Sending transaction #"%int) idx
                submitTxRaw na transaction
                liftIO $ modifyIORef' realTxNum (+1)

                endT <- getPosixMs
                let runDelta = endT - startT
                wait $ for $ ms (max 0 $ tpsDelta - runDelta)

                -- put timestamp to current txmap
                liftIO $ registerSentTx txTimestamps curTxId $ fromIntegral startT * 1000

        finishT <- getPosixMs
        realTxNumVal <- liftIO $ readIORef realTxNum

        let globalTime, realTPS :: Double
            globalTime = (fromIntegral (finishT - beginT)) / 1000
            realTPS = (fromIntegral realTxNumVal) / globalTime
            (newTPS, newStep) = if realTPS >= goTPS - 5
                                then (goTPS + increaseStep, increaseStep)
                                else (realTPS, increaseStep / 2)

        putText "----------------------------------------"
--        putText "wrote json to ./timestampsTxSender.json"
--        liftIO $ LBS.writeFile "timestampsTxSender.json" $
--            encode $ M.toList resMap
        putText $ "Sending transactions took (s): " <> show globalTime
        putText $ "So real tps was: " <> show realTPS

        -- We collect tables of really generated tps
        liftIO $ appendFile tpsCsvFile $
            tpsCsvFormat (globalTime, goTPS, realTPS)

        return (newTPS, newStep)

    vers <- liftIO $ dumpTxTable txTimestamps
    liftIO $ appendFile verifyCsvFile $
        mconcat $ map verifyCsvFormat vers

verifyCsvFile, tpsCsvFile :: FilePath
verifyCsvFile = "smart-gen-verifications.csv"
tpsCsvFile = "smart-gen-tps.csv"

verifyCsvHeader, tpsCsvHeader :: Text
tpsCsvHeader = "global_time,round_tps,real_tps\n"
verifyCsvHeader = "transaction_id,sending_ts,verification_ts\n"

tpsCsvFormat :: (Double, Double, Double) -> Text
tpsCsvFormat (gtime, roundTPS, realTPS) =
    sformat (fixed 2%","%fixed 2%","%fixed 2%"\n") gtime roundTPS realTPS

verifyCsvFormat :: (TxId, Word64, Word64) -> Text
verifyCsvFormat (txId, sendTs, verifyTs) =
    sformat (build%","%int%","%int%"\n") txId sendTs verifyTs

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
            , lpHandlerPrefix = goLogsPrefix
            , lpConfigPath    = goLogConfig
            }
        baseParams =
            BaseParams
            { bpLoggingParams      = logParams
            , bpPort               = 24962 + fromIntegral goGenesisIdx
            , bpDHTPeers           = goDHTPeers
            , bpDHTKeyOrType       = Right DHTFull
            , bpDHTExplicitInitial = goDhtExplicitInitial
            }
        stakesDistr = case (goFlatDistr, goBitcoinDistr) of
            (Nothing, Nothing) -> def
            (Just _, Just _) ->
                panic "flat-distr and bitcoin distr are conflicting options"
            (Just (nodes, coins), Nothing) ->
                FlatStakes (fromIntegral nodes) (fromIntegral coins)
            (Nothing, Just (nodes, coins)) ->
                BitcoinStakes (fromIntegral nodes) (fromIntegral coins)

    bracketDHTInstance baseParams $ \inst -> do
        let timeSlaveParams =
                baseParams
                { bpLoggingParams = logParams { lpRunnerTag = "time-slave" }
                }

        systemStart <- runTimeSlaveReal inst timeSlaveParams

        let params =
                NodeParams
                { npDbPath      = Nothing
                , npRebuildDb   = False
                , npSystemStart = systemStart
                , npSecretKey   = sk
                , npVssKeyPair  = vssKeyPair
                , npBaseParams  = baseParams
                , npCustomUtxo  = Just $ genesisUtxo stakesDistr
                , npTimeLord    = False
                , npJLFile      = goJLFile
                }

        case goSscAlgo of
            GodTossingAlgo -> putText "Using MPC coin tossing" *>
                              runSmartGen @SscGodTossing inst params opts
            NistBeaconAlgo -> putText "Using NIST beacon" *>
                              runSmartGen @SscNistBeacon inst params opts
