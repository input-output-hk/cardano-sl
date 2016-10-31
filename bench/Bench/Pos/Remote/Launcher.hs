module Bench.Pos.Remote.Launcher
       ( startSupporter
       , startFullNode
       , NodeNumber
       ) where

import           Control.TimeWarp.Logging (LoggerName (..), Severity (Debug))
import           Data.Default             (def)
import           Data.List                ((!!))
import qualified Data.Map.Strict          as M
import           Data.Maybe               (fromJust)
import           Formatting               (int, sformat, stext, (%))
import           Text.Parsec              (parse)
import           Universum

import           Bench.Pos.Remote.Config  (FullNodeConfig (..), SupporterConfig (..),
                                           readRemoteConfig)
import           Pos.CLI                  (dhtKeyParser, dhtNodeParser)
import           Pos.Crypto               (unsafeHash)
import           Pos.DHT                  (DHTNodeType (..), dhtNodeType)
import           Pos.Genesis              (genesisAddresses, genesisSecretKeys,
                                           genesisVssKeyPairs)
import           Pos.Launcher             (BaseParams (..), LoggingParams (..),
                                           NodeParams (..), getCurTimestamp, runNodeStats,
                                           runSupporterReal)
import           Pos.Types                (TxOut (..), Utxo)


type NodeNumber = Int

-- | For every static stakeholder it generates `k` coins, but in `k`
-- transaction (1 coin each), where `k` is input parameter.
utxoPetty :: Int -> Utxo
utxoPetty k =
    M.fromList $ flip concatMap genesisAddresses $ \a ->
        map (\i -> ((unsafeHash (show a ++ show i), 0), TxOut a 1)) [1..k]

-- TODO: move to some util library (e. g. `serokell-core`)
eitherPanic :: Show a => Text -> Either a b -> b
eitherPanic msgPrefix = either (panic . (msgPrefix <>) . show) identity

startSupporter :: FilePath -> IO ()
startSupporter config = do
    SupporterConfig {..} <- readRemoteConfig config

    let dhtKey = eitherPanic "Invalid DHT key: " $ parse dhtKeyParser "" $ toString scDhtKey
        keyType = dhtNodeType dhtKey

    when (keyType /= Just DHTSupporter) $
        panic $ sformat ("Invalid type of DHT key: "%stext%" (should be `Just DHTSupporter`)") $ show keyType

    let logging = def
                  { lpRootLogger = "supporter"
                  , lpMainSeverity = Debug
                  }
        params = BaseParams
                 { bpLogging = logging
                 , bpPort = scPort
                 , bpDHTPeers = []
                 , bpDHTKeyOrType = Left dhtKey
                 }

    runSupporterReal params

startFullNode :: FilePath -> NodeNumber -> IO ()
startFullNode config nodeNumber = do
    when (nodeNumber > 41 || nodeNumber < 0) $
        panic $ sformat ("Invalid node number "%int%" (should be in range [0..41])") nodeNumber

    FullNodeConfig {..} <- readRemoteConfig config

    curTime <- getCurTimestamp
    let startTime = fromJust $ fromIntegral <$> fncStartTime <|> Just curTime

    let dhtSupporter = eitherPanic "Invalid supporter address: " $ parse dhtNodeParser "" $ toString fncSupporterAddr
        logging = def
                  { lpRootLogger = LoggerName ("fullnode." ++ show nodeNumber)
                  , lpMainSeverity = Debug
                  }
        baseParams =
            BaseParams
            { bpLogging      = logging
            , bpPort         = fncPort
            , bpDHTPeers     = [dhtSupporter]
            , bpDHTKeyOrType = Right DHTFull
            }
        params =
            NodeParams
            { npDbPath       = fncDbPath
            -- always start with a fresh database (maybe will change later)
            , npRebuildDb    = True
            , npSystemStart  = startTime
            , npSecretKey    = genesisSecretKeys !! nodeNumber
            , npVssKeyPair   = genesisVssKeyPairs !! nodeNumber
            , npBaseParams   = baseParams
            , npCustomUtxo   = Just $ utxoPetty 10000
            }

    runNodeStats params
