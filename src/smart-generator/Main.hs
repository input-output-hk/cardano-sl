{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Lens           ((^.))
import           Control.Monad          (when)
import           Control.TimeWarp.Timed (for, fork_, ms, repeatForever, sec, wait)
import           Data.Aeson             (encode)
import           Data.Array.IO          (IOArray)
import           Data.Array.MArray      (MArray (..), newListArray, readArray, writeArray)
import qualified Data.ByteString.Lazy   as LBS
import           Data.Default           (def)
import qualified Data.HashMap.Strict    as M
import           Data.IORef             (IORef, modifyIORef', newIORef, readIORef,
                                         writeIORef)
import           Data.List              (head, intersect, tail, (!!))
import           Data.Monoid            ((<>))
import           Data.Text.IO           (writeFile)
import           Data.Time.Clock.POSIX  (getPOSIXTime)
import           Formatting             (build, fixed, float, int, sformat, (%))
import           Options.Applicative    (Parser, ParserInfo, auto, execParser, fullDesc,
                                         help, helper, info, long, many, metavar, option,
                                         progDesc, short, showDefault, switch, value)
import           Serokell.Util.OptParse (fromParsec, strOption)
import           System.Wlog            (logInfo, logWarning)
import           Test.QuickCheck        (arbitrary, generate)
import           Universum              hiding (head, (<>))

import           Pos.CLI                (dhtNodeParser, sscAlgoParser)
import           Pos.Communication      (allListeners)
import           Pos.Constants          (k, slotDuration)
import           Pos.Crypto             (KeyPair (..), SecretKey, hash, sign, unsafeHash)
import           Pos.DHT                (DHTNode, DHTNodeType (..), ListenerDHT, dhtAddr,
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

-----------------------------------------------------------------------
-- CLI options
-----------------------------------------------------------------------
data GenOptions = GenOptions
    { goGenesisIdx         :: !Word       -- ^ Index in genesis key pairs.
    -- , goRemoteAddr  :: !NetworkAddress -- ^ Remote node address
    , goDHTPeers           :: ![DHTNode]  -- ^ Initial DHT nodes
    , goRoundDuration      :: !Double     -- ^ Number of seconds per round
    , goTxFrom             :: !Int        -- ^ Start from UTXO transaction #x
    , goInitBalance        :: !Int        -- ^ Total coins in init utxo per address
    , goTPSs               :: ![Double]   -- ^ TPS rate
    , goPropThreshold      :: !Int
    , goSingleRecipient    :: !Bool       -- ^ Send to only 1 node if flag is set
    , goDhtExplicitInitial :: !Bool
    , goLogConfig          :: !(Maybe FilePath)
    , goLogsPrefix         :: !(Maybe FilePath)
    , goJLFile             :: !(Maybe FilePath)
    , goSscAlgo            :: !SscAlgo
    , goFlatDistr          :: !(Maybe (Int, Int))
    , goBitcoinDistr       :: !(Maybe (Int, Int))
    }

optionsParser :: Parser GenOptions
optionsParser = GenOptions
    <$> option auto
            (short 'i'
          <> long "index"
          <> metavar "INT"
          <> help "Index in list of genesis key pairs")
    -- <*> option (fromParsec addrParser)
    --         (long "peer"
    --       <> metavar "HOST:PORT"
    --       <> help "Node address to ZERG RUSH")
    <*> many (option (fromParsec dhtNodeParser) $
             long "peer"
          <> metavar "HOST:PORT/HOST_ID"
          <> help "Initial DHT peer (may be many)")
    <*> option auto
            (short 'd'
          <> long "round-duration"
          <> help "Duration of one testing round")
    <*> option auto
            (long "tx-from-n"
          <> value 0
          <> help "From which transaction in utxo to start")
    <*> option auto
            (long "init-money"
          <> help "How many coins node has in the beginning")
    <*> many (option auto $
             short 't'
          <> long "tps"
          <> metavar "DOUBLE"
          <> help "TPS (transactions per second)")
    <*> option auto
            (short 'P'
          <> long "propagate-threshold"
          <> value 1
          <> help "Approximate number of slots needed to propagate transactions across the network")
    <*> switch
        (long "single-recipient" <>
         help "Send transactions only to one of nodes")
    <*> switch
        (long "explicit-initial" <>
         help
             "Explicitely contact to initial peers as to neighbors (even if they appeared offline once)")
    <*> optional (strOption $
                  long "log-config"
               <> metavar "FILEPATH"
               <> help "Path to logger configuration")
    <*> optional (strOption $
                  long "logs-prefix"
               <> metavar "FILEPATH"
               <> help "Prefix to logger output path")
    <*> optional (strOption $
                  long "json-log"
               <> metavar "FILEPATH"
               <> help "Path to json log file")
    <*> option (fromParsec sscAlgoParser)
        (long "ssc-algo"
      <> metavar "ALGO"
      <> value GodTossingAlgo
      <> showDefault
      <> help "Shared Seed Calculation algorithm which nodes will use")
    <*> optional
        (option auto $
         mconcat
            [ long "flat-distr"
            , metavar "(INT,INT)"
            , help "Use flat stake distribution with given parameters (nodes, coins)"
            ])
    <*> optional
        (option auto $
         mconcat
            [ long "bitcoin-distr"
            , metavar "(INT,INT)"
            , help "Use bitcoin stake distribution with given parameters (nodes, coins)"
            ])

optsInfo :: ParserInfo GenOptions
optsInfo = info (helper <*> optionsParser) $
    fullDesc `mappend` progDesc "Stupid transaction generator"

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
    let maxTps = round $ maximum goTPSs
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

runSmartGen :: forall ssc . SscConstraint ssc
            => KademliaDHTInstance -> NodeParams -> GenOptions -> IO ()
runSmartGen inst np@NodeParams{..} opts@GenOptions{..} =
    runRealMode inst np (realListeners @ssc np) $ getNoStatsT $ do
    let i = fromIntegral goGenesisIdx
        getPosixMs = round . (*1000) <$> liftIO getPOSIXTime
        totalRounds = length goTPSs
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

    logInfo "Issuing seed transaction"
    submitTxRaw na initTx
    logInfo "Waiting for verifying period..."
    wait $ for $ fromIntegral (k + goPropThreshold) * slotDuration

    tpsTable <- forM (zip [1..] goTPSs) $ \(roundNum :: Int, goTPS) -> do
        logInfo $ sformat ("Round "%int%" from "%int%": TPS "%float)
            roundNum totalRounds goTPS

        let tpsDelta = round $ 1000 / goTPS
            txNum = round $ goRoundDuration * goTPS

        beginT <- getPosixMs
        resMap <- foldM
            (\curmap (idx :: Int) -> do
                preStartT <- getPosixMs
                -- prevent periods longer than we expected
                if preStartT - beginT > round (goRoundDuration * 1000)
                then pure curmap
                else do
                    transaction <- nextValidTx bambooPool tpsDelta
                    let curTxId = hash transaction

                    startT <- getPosixMs
                    logInfo $ sformat ("Sending transaction #"%int) idx
                    submitTxRaw na transaction

                    -- sometimes nodes fail so we never write timestamps...
                    when (idx `mod` 271 == 0) $ void $ liftIO $ forkIO $
                        LBS.writeFile "timestampsTxSender.json" $
                        encode $ M.toList curmap

                    endT <- getPosixMs
                    let runDelta = endT - startT
                    wait $ for $ ms (max 0 $ tpsDelta - runDelta)

                    -- put timestamp to current txmap
                    liftIO $ registerSentTx txTimestamps curTxId $ fromIntegral startT * 1000

                    -- we dump microseconds to be consistent with JSON log
                    pure $ M.insert (pretty curTxId) (startT * 1000) curmap)
            (M.empty :: M.HashMap Text Int) -- TxId Int
            [0 .. txNum - 1]

        finishT <- getPosixMs

        let globalTime, realTPS :: Double
            globalTime = (fromIntegral (finishT - beginT)) / 1000
            realTPS = (fromIntegral txNum) / globalTime

        putText "----------------------------------------"
        putText "wrote json to ./timestampsTxSender.json"
        liftIO $ LBS.writeFile "timestampsTxSender.json" $
            encode $ M.toList resMap
        putText $ "Sending transactions took (s): " <> show globalTime
        putText $ "So real tps was: " <> show realTPS

        -- We collect tables of really generated tps
        return (globalTime, goTPS, realTPS)

    vers <- liftIO $ dumpTxTable txTimestamps
    liftIO $ writeFile "smart-gen-verifications.csv" $ verificationsToCsv vers
    liftIO $writeFile "smart-gen-tps.csv" $ tpsToCsv tpsTable

tpsToCsv :: [(Double, Double, Double)] -> Text
tpsToCsv entries =
    "global_time,round_tps,real_tps\n" <>
    mconcat (map formatter entries)
  where
    formatter (gtime, roundTPS, realTPS) =
        sformat (fixed 2%","%fixed 2%","%fixed 2%"\n") gtime roundTPS realTPS

verificationsToCsv :: [(TxId, Word64, Word64)] -> Text
verificationsToCsv entries =
    "transaction_id,sending_ts,verification_ts\n" <>
    mconcat (map formatter entries)
  where
    formatter (txId, sendTs, verifyTs) =
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
