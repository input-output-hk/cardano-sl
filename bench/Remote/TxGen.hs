{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.TimeWarp.Logging (Severity (Debug), logInfo)
import           Control.TimeWarp.Rpc     (NetworkAddress)
import           Data.Default             (def)
import           Data.List                ((!!))
import           Formatting               (build, sformat, (%))
import           Options.Applicative      (Parser, ParserInfo, auto, execParser, fullDesc,
                                           help, helper, info, long, metavar, option,
                                           progDesc, short, value)
import           Universum                hiding ((<>))

import           Data.Monoid              ((<>))

import           Pos.CLI                  (addrParser)
import           Pos.Communication        (sendTx)
import           Pos.Crypto               (SecretKey, hash, sign, unsafeHash)
import           Pos.DHT                  (DHTNode, DHTNodeType (..))
import           Pos.Genesis              (genesisAddresses, genesisSecretKeys,
                                           genesisVssKeyPairs)
import           Pos.Launcher             (BaseParams (..), LoggingParams (..),
                                           NodeParams (..), runRealMode)
import           Pos.Statistics           (getNoStatsT)
import           Pos.Types                (Address, Coin, Timestamp (Timestamp), Tx (..),
                                           TxId, TxIn (..), TxOut (..), Utxo, timestampF,
                                           txF)
import           Pos.WorkMode             (RealMode, ServiceMode, WorkMode,
                                           getNodeContext, ncSecretKey)
import           Serokell.Util.OptParse   (fromParsec)

data GenOptions = GenOptions
    { goGenesisIdx :: !Word   -- ^ Index in genesis key pairs.
    , goRemoteAddr :: !NetworkAddress -- ^ Remote node address
    , goTxNum      :: !Word -- ^ Number of tx to send
    , goRemoteMode :: !RemoteMode
    }

-- | Determines how to generate initial transaction
data RemoteMode = Prod      -- ^ as in `genesisUtxo`
                | Bench Int -- ^ as in `utxoPetty k`
                deriving (Read, Show)

optionsParser :: Parser GenOptions
optionsParser = GenOptions
    <$> option auto
            (mconcat
                 [ short 'i'
                 , long "index"
                 , metavar "INT"
                 , help "Index in list of genesis key pairs"
                 ])
    <*> option (fromParsec addrParser)
            (long "peer"
          <> metavar "HOST:PORT"
          <> help "Node address to ZERG RUSH")
    <*> option auto
            (short 'n'
          <> long "tx-number"
          <> value 10000
          <> help "Num of transactions (def 10000)")
    <*> option auto
            (short 'm'
          <> long "remote-mode"
          <> value Prod
          <> help "`Prod` if rushing regular node, `Bench <k>` - for benchmark node with `k` coins in genesis utxo")

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

generateTxList :: WorkMode m => RemoteMode -> Address -> Int -> m [Tx]
generateTxList mode addr idx = do
    sk <- ncSecretKey <$> getNodeContext
    return []

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
                             , bpPort = 24962
                             , bpDHTPeers = []
                             , bpDHTKeyOrType = Right DHTClient
                             }
            , npCustomUtxo = Nothing
            }
        addr = genesisAddresses !! i

    runRealMode params [] $ getNoStatsT $ do
        txs <- generateTxList goRemoteMode addr i
        forM_ txs $ \tx -> do
            submitTxRaw goRemoteAddr tx
            -- delay
