{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.TimeWarp.Logging (Severity (Debug), logInfo)
import           Control.TimeWarp.Rpc     (NetworkAddress)
import           Control.TimeWarp.Timed   (for, ms, wait)
import           Data.Aeson               (encode)
import qualified Data.ByteString.Lazy     as LBS
import           Data.Default             (def)
import qualified Data.HashMap.Strict      as M
import           Data.IORef               (modifyIORef, newIORef, readIORef)
import           Data.List                ((!!))
import           Data.Monoid              ((<>))
import           Data.Time.Clock.POSIX    (getPOSIXTime)
import           Formatting               (build, float, int, sformat, (%))
import           Options.Applicative      (Parser, ParserInfo, auto, execParser, fullDesc,
                                           help, helper, info, long, many, metavar,
                                           option, progDesc, short, switch, value)
import           System.Random            (getStdGen, randomRs)
import           Universum                hiding ((<>))

import           Pos.CLI                  (dhtNodeParser)
import           Pos.Communication        (sendTx)
import           Pos.Crypto               (hash, unsafeHash)
import           Pos.DHT                  (DHTNode, DHTNodeType (..), dhtAddr,
                                           discoverPeers)
import           Pos.Genesis              (genesisAddresses, genesisSecretKeys)
import           Pos.Launcher             (BaseParams (..), LoggingParams (..),
                                           NodeParams (..), bracketDHTInstance,
                                           runRealMode, submitTx)
import           Pos.Ssc.DynamicState     (genesisVssKeyPairs)
import           Pos.Statistics           (getNoStatsT)
import           Pos.Types                (Tx (..), TxId, txF)
import           Pos.Util.JsonLog         ()
import           Pos.WorkMode             (WorkMode)
import           Serokell.Util.OptParse   (fromParsec)
import           Pos.Ssc.DynamicState       (SscDynamicState)

data GenOptions = GenOptions
    { goGenesisIdx         :: !Word       -- ^ Index in genesis key pairs.
    -- , goRemoteAddr  :: !NetworkAddress -- ^ Remote node address
    , goDHTPeers           :: ![DHTNode]  -- ^ Initial DHT nodes
    , goRoundDuration      :: !Double     -- ^ Number of seconds per round
    , goTxFrom             :: !Int        -- ^ Start from UTXO transaction #x
    , goInitBalance        :: !Int        -- ^ Total coins in init utxo per address
    , goTPSs               :: ![Double]   -- ^ TPS rate
    , goDhtExplicitInitial :: !Bool
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
        (long "explicit-initial" <>
         help
             "Explicitely contact to initial peers as to neighbors (even if they appeared offline once)")

optsInfo :: ParserInfo GenOptions
optsInfo = info (helper <*> optionsParser) $
    fullDesc `mappend` progDesc "Stupid transaction generator"

-- | Send the ready-to-use transaction
submitTxRaw :: WorkMode ssc m => NetworkAddress -> Tx -> m ()
submitTxRaw na tx = do
    let txId = hash tx
    logInfo $ sformat ("Submitting transaction: "%txF) tx
    logInfo $ sformat ("Transaction id: "%build) txId
    sendTx na tx

-- generateTxList :: WorkMode m => RemoteMode -> Address -> Int -> m [Tx]
-- generateTxList mode addr balance = do
--     gen <- liftIO getStdGen
--     sk <- ncSecretKey <$> getNodeContext

--     -- TODO: genesisN is currently 3, which breaks everything
--     let nodesN = length genesisAddresses
--         -- send everybody per 1 coin randomly
--         recAddrs = take balance $ map (genesisAddresses !!) $ randomRs (0, nodesN - 1) gen
--         makeTx txOutAddress idx =
--             let txOutValue = 1
--                 txInHash = initTxId idx
--                 txInIndex = 0
--                 txOutputs = [TxOut {..}]
--                 txInputs = [TxIn { txInSig = sign sk (txInHash, txInIndex, txOutputs), .. }]
--             in Tx {..}
--         txs = zipWith makeTx recAddrs [0..]
--         initTxId :: Int -> TxId
--         initTxId = case mode of
--             Prod  -> \_ -> unsafeHash addr
--             Bench -> \k -> unsafeHash (show addr ++ show k)

--     return txs

main :: IO ()
main = do
    GenOptions {..} <- execParser optsInfo
    let i = fromIntegral goGenesisIdx
        logParams =
            def
            { lpMainSeverity = Debug
            , lpRootLogger = "tx-gen"
            }
        baseParams =
            BaseParams
            { bpLogging            = logParams
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
            , npVssKeyPair  = genesisVssKeyPairs !! i
            , npBaseParams  = baseParams
            , npCustomUtxo  = Nothing
            , npTimeLord    = False
            , npJLFile      = Nothing
            }
        addr = genesisAddresses !! i

    gen <- getStdGen
    let nodesN = length genesisAddresses
        recAddrs = take goInitBalance $ map (genesisAddresses !!) $ randomRs (0, nodesN - 1) gen
        initTxId :: Int -> TxId
        initTxId k = unsafeHash (show addr ++ show k)

    let getPosixMs = round . (*1000) <$> liftIO getPOSIXTime
        totalRounds = length goTPSs

    curRoundOffset <- newIORef 0

    bracketDHTInstance baseParams $ \inst -> do
        runRealMode @SscDynamicState inst params [] $ getNoStatsT $ do
            logInfo "TX GEN RUSHING"
            peers <- discoverPeers DHTFull

            let na = dhtAddr <$> peers

            forM_ (zip [1..] goTPSs) $ \(roundNum :: Int, goTPS) -> do
                logInfo $ sformat ("Round "%int%" from "%int%": TPS "%float)
                    roundNum totalRounds goTPS

                roundOffset <- liftIO $ readIORef curRoundOffset

                let tpsDelta = round $ 1000 / goTPS
                    txNum = round $ goRoundDuration * goTPS
                    indices = [roundOffset .. roundOffset + txNum - 1]
                    curRecAddrs = take txNum $ drop roundOffset recAddrs

                liftIO $ modifyIORef curRoundOffset (+ txNum)

                beginT <- getPosixMs
                resMap <- foldrM
                    (\(recAddr, idx) curmap -> do
                        startT <- getPosixMs

                        logInfo $ sformat ("Sending transaction #"%int) idx
                        logInfo $ sformat ("Recipient address: "%build) recAddr
                        tx <- submitTx na (initTxId (idx + goTxFrom), 0) (recAddr, 1)
                        -- sometimes nodes fail so we never write timestamps...
                        when (idx `mod` 271 == 0) $ void $ liftIO $ forkIO $
                            LBS.writeFile "timestampsTxSender.json" $
                                encode $ M.toList curmap

                        endT <- getPosixMs
                        let runDelta = endT - startT
                        wait $ for $ ms (max 0 $ tpsDelta - runDelta)

                        -- we dump microseconds to be consistent with JSON log
                        pure $ M.insert (pretty $ hash tx) (startT * 1000) curmap)
                    (M.empty :: M.HashMap Text Int) -- TxId Int
                    (zip curRecAddrs indices)
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
