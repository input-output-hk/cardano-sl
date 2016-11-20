{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.TimeWarp.Timed (for, fork_, ms, sec, wait)
import           Data.Aeson             (encode)
import           Data.Array.IO          (IOArray)
import           Data.Array.MArray      (MArray (..), newListArray, readArray, writeArray)
import qualified Data.ByteString.Lazy   as LBS
import qualified Data.HashMap.Strict    as M
import           Data.IORef             (IORef, modifyIORef, newIORef, readIORef,
                                         writeIORef)
import           Data.List              (tail)
import           Data.List              ((!!))
import           Data.Monoid            ((<>))
import           Data.Time.Clock.POSIX  (getPOSIXTime)
import           Formatting             (float, int, sformat, (%))
import           Options.Applicative    (Parser, ParserInfo, auto, execParser, fullDesc,
                                         help, helper, info, long, many, metavar, option,
                                         progDesc, short, switch, value)
import           Serokell.Util.OptParse (fromParsec)
import           System.Wlog            (logInfo)
import           Test.QuickCheck        (arbitrary, generate)
import           Universum              hiding ((<>))

import           Pos.CLI                (dhtNodeParser)
import           Pos.Constants          (k, slotDuration)
import           Pos.Crypto             (KeyPair (..), SecretKey, hash, sign, unsafeHash)
import           Pos.DHT                (DHTNode, DHTNodeType (..), dhtAddr,
                                         discoverPeers)
import           Pos.Genesis            (genesisAddresses, genesisSecretKeys)
import           Pos.Launcher           (BaseParams (..), LoggingParams (..),
                                         NodeParams (..), bracketDHTInstance, runNode,
                                         runRealMode, submitTxRaw)
import           Pos.Ssc.GodTossing     (SscGodTossing)
import           Pos.Statistics         (getNoStatsT)
import           Pos.Types              (Address, Tx (..), TxId, TxIn (..), TxOut (..))
import           Pos.Util.JsonLog       ()

data GenOptions = GenOptions
    { goGenesisIdx         :: !Word       -- ^ Index in genesis key pairs.
    -- , goRemoteAddr  :: !NetworkAddress -- ^ Remote node address
    , goDHTPeers           :: ![DHTNode]  -- ^ Initial DHT nodes
    , goRoundDuration      :: !Double     -- ^ Number of seconds per round
    , goTxFrom             :: !Int        -- ^ Start from UTXO transaction #x
    , goInitBalance        :: !Int        -- ^ Total coins in init utxo per address
    , goTPSs               :: ![Double]   -- ^ TPS rate
    , goAlgoK              :: !Int
    , goPropThreshold      :: !Int
    , goSingleRecipient    :: !Bool       -- ^ Send to only 1 node if flag is set
    , goDhtExplicitInitial :: !Bool
    , goLogConfig          :: !(Maybe FilePath)
    , goLogsPrefix         :: !(Maybe FilePath)
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
            (short 'k'
          <> long "verify-k"
          <> value k
          <> help "Number of blocks to consider transaction verified")
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
    <*> optional (option auto $
                 long "log-config"
              <> metavar "FILEPATH"
              <> help "Path to logger configuration")
    <*> optional (option auto $
                 long "logs-prefix"
              <> metavar "FILEPATH"
              <> help "Prefix to logger output path")


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
        n' = maxTps * (goAlgoK + goPropThreshold) * fromIntegral (slotDuration `div` sec 1)
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
          bamboos = map (genChain sk addr (hash tx) . fromIntegral) [0 .. outputsN - 1]

fetchTx :: BambooPool -> IO Tx
fetchTx BambooPool {..} = do
    idx <- readIORef bpCurIdx
    lastChainIdx <- snd <$> getBounds bpChains
    chain <- readArray bpChains idx
    writeArray bpChains idx $ tail chain
    writeIORef bpCurIdx $ (idx + 1) `mod` (lastChainIdx + 1)
    return $ chain !! 0

main :: IO ()
main = do
    opts@GenOptions {..} <- execParser optsInfo

    -- | Use arbitrary key/value pair to not have any stake
    KeyPair _ sk <- generate arbitrary

    -- | Same for VssKeyPair
    vssKeyPair <- generate arbitrary

    let i = fromIntegral goGenesisIdx
        logParams =
            LoggingParams
            { lpRunnerTag     = "tx-gen"
            , lpHandlerPrefix = goLogsPrefix
            , lpConfigPath    = goLogConfig
            }
        baseParams =
            BaseParams
            { bpLoggingParams      = logParams
            , bpPort               = 24962 + fromIntegral i
            , bpDHTPeers           = goDHTPeers
            , bpDHTKeyOrType       = Right DHTFull
            , bpDHTExplicitInitial = goDhtExplicitInitial
            }
        params =
            NodeParams
            { npDbPath      = Nothing
            , npRebuildDb   = False
            , npSystemStart = 1477706355381569 --arbitrary value
            , npSecretKey   = sk
            , npVssKeyPair  = vssKeyPair
            , npBaseParams  = baseParams
            , npCustomUtxo  = Nothing
            , npTimeLord    = False
            , npJLFile      = Nothing
            }
        getPosixMs = round . (*1000) <$> liftIO getPOSIXTime
        totalRounds = length goTPSs
        initTx = initTransaction opts

    bambooPool <- createBambooPool (genesisSecretKeys !! i) (genesisAddresses !! i) initTx

    bracketDHTInstance baseParams $ \inst -> do
        runRealMode @SscGodTossing inst params [] $ getNoStatsT $ do
            -- | Run all the usual node workers in order to get
            -- access to blockchain

            fork_ runNode

            logInfo "STARTING TXGEN"
            peers <- discoverPeers DHTFull

            let na' = dhtAddr <$> peers
                na = if goSingleRecipient
                     then take 1 na'
                     else na'

            logInfo "Issuing seed transaction"
            submitTxRaw na initTx
            -- logInfo "Waiting for verifying period..."
            -- wait $ for $ fromIntegral (goAlgoK + goPropThreshold) * slotDuration

            forM_ (zip [1..] goTPSs) $ \(roundNum :: Int, goTPS) -> do
                logInfo $ sformat ("Round "%int%" from "%int%": TPS "%float)
                    roundNum totalRounds goTPS

                let tpsDelta = round $ 1000 / goTPS
                    txNum = round $ goRoundDuration * goTPS

                beginT <- getPosixMs
                resMap <- foldM
                    (\curmap (idx :: Int) -> do
                        startT <- getPosixMs

                        transaction <- liftIO $ fetchTx bambooPool
                        logInfo $ sformat ("Sending transaction #"%int) idx
                        submitTxRaw na transaction

                        -- sometimes nodes fail so we never write timestamps...
                        when (idx `mod` 271 == 0) $ void $ liftIO $ forkIO $
                            LBS.writeFile "timestampsTxSender.json" $
                                encode $ M.toList curmap

                        endT <- getPosixMs
                        let runDelta = endT - startT
                        wait $ for $ ms (max 0 $ tpsDelta - runDelta)

                        -- we dump microseconds to be consistent with JSON log
                        pure $ M.insert (pretty $ hash transaction) (startT * 1000) curmap)
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

