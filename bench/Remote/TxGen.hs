{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.TimeWarp.Logging (Severity (Debug), logInfo)
import           Control.TimeWarp.Rpc     (NetworkAddress)
import           Control.TimeWarp.Timed   (Millisecond, for, wait)
import           Data.Default             (def)
import           Data.List                ((!!))
import           Data.Monoid              ((<>))
import           Formatting               (build, int, sformat, (%))
import           Options.Applicative      (Parser, ParserInfo, auto, execParser, fullDesc,
                                           help, helper, info, long, many, metavar,
                                           option, progDesc, short, value)
import           System.Random            (getStdGen, randomRs)
import           Universum                hiding ((<>))

import           Pos.CLI                  (addrParser, dhtNodeParser)
import           Pos.Communication        (sendTx)
import           Pos.Crypto               (SecretKey, hash, sign, unsafeHash)
import           Pos.DHT                  (DHTNode, DHTNodeType (..), dhtAddr,
                                           discoverPeers, filterByNodeType, getKnownPeers)
import           Pos.Genesis              (genesisAddresses, genesisSecretKeys,
                                           genesisVssKeyPairs)
import           Pos.Launcher             (BaseParams (..), LoggingParams (..),
                                           NodeParams (..), runRealMode, submitTx)
import           Pos.Statistics           (getNoStatsT)
import           Pos.Types                (Address, Coin, Timestamp (Timestamp), Tx (..),
                                           TxId, TxIn (..), TxOut (..), Utxo, timestampF,
                                           txF)
import           Pos.WorkMode             (RealMode, ServiceMode, WorkMode,
                                           getNodeContext, ncSecretKey)
import           Serokell.Util.OptParse   (fromParsec)

data GenOptions = GenOptions
    { goGenesisIdx  :: !Word           -- ^ Index in genesis key pairs.
    -- , goRemoteAddr  :: !NetworkAddress -- ^ Remote node address
    , goDHTPeers    :: [DHTNode]       -- ^ Initial DHT nodes
    , goTxNum       :: !Int            -- ^ Number of tx to send
    , goInitBalance :: !Int            -- ^ Total coins in init utxo per address
    , goDelay       :: !Millisecond    -- ^ Delay between transactions
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
            (short 'n'
          <> long "tx-number"
          <> value 10000
          <> help "Num of transactions (def 10000)")
   <*> option auto
            (short 'k'
          <> long "init-money"
          <> value 10000
          <> help "How many coins node has in the beginning")
    <*> option auto
            (short 'd'
          <> long "delay"
          <> metavar "INT"
          <> value 500
          <> help "Delay between transactions in ms")

optsInfo :: ParserInfo GenOptions
optsInfo = info (helper <*> optionsParser) $
    fullDesc `mappend` progDesc "Stupid transaction generator"

-- | Send the ready-to-use transaction
submitTxRaw :: WorkMode m => NetworkAddress -> Tx -> m ()
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
        params =
            NodeParams
            { npDbPath = Nothing
            , npRebuildDb = False
            , npSystemStart = 1477706355381569 --arbitrary value
            , npSecretKey = genesisSecretKeys !! i
            , npVssKeyPair = genesisVssKeyPairs !! i
            , npBaseParams = BaseParams
                             { bpLogging = def { lpMainSeverity = Debug, lpRootLogger = "tx-gen" }
                             , bpPort = 24962 + fromIntegral i
                             , bpDHTPeers = goDHTPeers
                             , bpDHTKeyOrType = Right DHTClient
                             }
            , npCustomUtxo = Nothing
            }
        addr = genesisAddresses !! i

    gen <- getStdGen
    let nodesN = length genesisAddresses
        recAddrs = take goInitBalance $ map (genesisAddresses !!) $ randomRs (0, nodesN - 1) gen
        initTxId :: Int -> TxId
        initTxId k = unsafeHash (show addr ++ show k)

    runRealMode params [] $ getNoStatsT $ do
        logInfo "TX GEN RUSHING"
        peers <- discoverPeers DHTFull

        let na = dhtAddr <$> peers
        forM_ (zip recAddrs [0..]) $ \(recAddr, idx) -> do
            logInfo $ sformat ("Sending transaction #"%int) idx
            logInfo $ sformat ("Recipient address: "%build) recAddr
            submitTx na (initTxId idx, 0) (recAddr, 1)
            wait $ for goDelay
