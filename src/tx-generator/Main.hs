{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.TimeWarp.Timed (for, ms, wait)
import           Data.Aeson             (encode)
import qualified Data.ByteString.Lazy   as LBS
import qualified Data.HashMap.Strict    as M
import           Data.IORef             (modifyIORef, newIORef, readIORef)
import           Data.List              ((!!))
import           Data.Time.Clock.POSIX  (getPOSIXTime)
import           Formatting             (float, int, sformat, (%))
import           Options.Applicative    (Parser, ParserInfo, auto, execParser, fullDesc,
                                         help, helper, info, long, many, metavar, option,
                                         progDesc, short, switch, value)
import           Serokell.Util.OptParse (fromParsec)
import           System.Wlog            (logInfo)
import           Universum

import           Pos.CLI                (dhtNodeParser)
import           Pos.Crypto             (hash, sign, unsafeHash)
import           Pos.DHT                (DHTNode, DHTNodeType (..), dhtAddr,
                                         discoverPeers)
import           Pos.Genesis            (genesisAddresses, genesisSecretKeys)
import           Pos.Launcher           (BaseParams (..), LoggingParams (..),
                                         NodeParams (..), bracketDHTInstance,
                                         runRawRealMode, submitTxRaw)
import           Pos.Ssc.GodTossing     (GtParams (..), SscGodTossing, genesisVssKeyPairs)
import           Pos.Statistics         (getNoStatsT)
import           Pos.Types              (Tx (..), TxIn (..), TxOut (..))
import           Pos.Util.JsonLog       ()

data GenOptions = GenOptions
    { goGenesisIdx         :: !Word       -- ^ Index in genesis key pairs.
    -- , goRemoteAddr  :: !NetworkAddress -- ^ Remote node address
    , goDHTPeers           :: ![DHTNode]  -- ^ Initial DHT nodes
    , goRoundDuration      :: !Double     -- ^ Number of seconds per round
    , goTxFrom             :: !Int        -- ^ Start from chain transaction #NUM
    , goInitBalance        :: !Int        -- ^ Total coins in init utxo per address
    , goTPSs               :: ![Double]   -- ^ TPS rate
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
            (short 'k'
          <> long "init-money"
          <> help "How many coins node has in the beginning")
    <*> many (option auto $
             short 't'
          <> long "tps"
          <> metavar "DOUBLE"
          <> help "TPS (transactions per second)")
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

txChain :: Int -> [Tx]
txChain i = genChain $ unsafeHash addr
    where
      addr = genesisAddresses !! i
      secretKey = genesisSecretKeys !! i
      genChain txInHash =
          let txOutValue = 1
              txInIndex = 0
              txOutputs = [TxOut { txOutAddress = addr, ..}]
              txInputs = [TxIn { txInSig = sign secretKey (txInHash, txInIndex, txOutputs), .. }]
              resultTransaction = Tx {..}
          in resultTransaction : genChain (hash resultTransaction)

main :: IO ()
main = do
    GenOptions {..} <- execParser optsInfo

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
            , bpDHTKeyOrType       = Right DHTClient
            , bpDHTExplicitInitial = goDhtExplicitInitial
            }
        params =
            NodeParams
            { npDbPath      = Nothing
            , npRebuildDb   = False
            , npSystemStart = 1477706355381569 --arbitrary value
            , npSecretKey   = genesisSecretKeys !! i
            , npBaseParams  = baseParams
            , npCustomUtxo  = Nothing
            , npTimeLord    = False
            , npJLFile      = Nothing
            }
        gtParams =
            GtParams
            {
              gtpRebuildDb  = False
            , gtpDbPath     = Nothing
            , gtpSscEnabled = False
            , gtpVssKeyPair = genesisVssKeyPairs !! i
            }
        getPosixMs = round . (*1000) <$> liftIO getPOSIXTime
        totalRounds = length goTPSs

    leftTxs <- newIORef $ take goTxFrom $ zip [0..] $ txChain i

    bracketDHTInstance baseParams $ \inst -> do
        runRawRealMode @SscGodTossing inst params gtParams [] $ getNoStatsT $ do
            logInfo "TX GEN RUSHING"
            peers <- discoverPeers DHTFull

            let na' = dhtAddr <$> peers
                na = if goSingleRecipient
                     then take 1 na'
                     else na'

            forM_ (zip [1..] goTPSs) $ \(roundNum :: Int, goTPS) -> do
                logInfo $ sformat ("Round "%int%" from "%int%": TPS "%float)
                    roundNum totalRounds goTPS

                transactions <- liftIO $ readIORef leftTxs
                let tpsDelta = round $ 1000 / goTPS
                    txNum = round $ goRoundDuration * goTPS

                liftIO $ modifyIORef leftTxs (drop txNum)

                beginT <- getPosixMs
                resMap <- foldM
                    (\curmap (idx :: Int, transaction) -> do
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

                        -- we dump microseconds to be consistent with JSON log
                        pure $ M.insert (pretty $ hash transaction) (startT * 1000) curmap)
                    (M.empty :: M.HashMap Text Int) -- TxId Int
                    (take txNum transactions)

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
