import           Control.TimeWarp.Logging (LoggerName (..))
import           Data.Default             (def)
import           Data.List                ((!!))
import           Data.Maybe               (fromJust)
import           Data.Monoid              ((<>))
import           Formatting               (int, sformat, stext, (%))
import           Options.Applicative      (Parser, ParserInfo, auto, command, execParser,
                                           fullDesc, header, help, helper, info, long,
                                           metavar, option, progDesc, short, strOption,
                                           subparser, value)
import           Control.TimeWarp.Logging (Severity (Debug))
import           Text.Parsec              (parse)
import           Universum                hiding ((<>))

import           Pos.CLI                  (dhtKeyParser, dhtNodeParser)
import           Pos.DHT                  (DHTNodeType (..), dhtNodeType)
import           Pos.Genesis              (genesisSecretKeys, genesisVssKeyPairs)
import           Pos.Launcher             (BaseParams (..), LoggingParams (..),
                                           NodeParams (..), getCurTimestamp, runNodeStats,
                                           runSupporterReal)

import           Bench.Pos.Remote.Config  (FullNodeConfig (..), SupporterConfig (..),
                                           readRemoteConfig)

-- TODO: move to some util library (e. g. `serokell-core`)
eitherPanic :: Show a => Text -> Either a b -> b
eitherPanic msgPrefix = either (panic . (msgPrefix <>) . show) identity


data RemoteBenchOptions = RBO
    { nodeSpecificOptions :: NodeSpecificOptions
    , configFilePath      :: FilePath
    }

type NodeNumber = Int

data NodeSpecificOptions = FullNodeOptions { nodeNumber :: NodeNumber }
                         | SupporterOptions

fullNodeParser :: Parser NodeSpecificOptions
fullNodeParser = FullNodeOptions
    <$> option auto (long "node-index"
                  <> short 'n'
                  <> metavar "INDEX"
                  <> help "This node index in the list of nodes (must be in range 0..41)")

-- Leave it like this in case any additional parameters for supporter emerge
-- (they probably will)
supporterParser :: Parser NodeSpecificOptions
supporterParser = pure SupporterOptions

optParser :: Parser RemoteBenchOptions
optParser = RBO
    <$> subparser (command "full" (info fullNodeParser $ progDesc "Run full node")
                <> command "supporter" (info supporterParser $ progDesc "Run supporter node"))

    <*> strOption (long "config"
                <> short 'c'
                <> metavar "PATH_TO_CONFIG"
                <> value "remote.yaml"
                <> help "Path to YAML config file")

parseOptions :: ParserInfo RemoteBenchOptions
parseOptions = info (helper <*> optParser) $ fullDesc
    <> header   "pos-bench-remote - distributed benchmarks for Cardano PoS"
    <> progDesc "Runs PoS full node and starts benchmarking transactions"


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
            , npRebuildDb    = True             -- always start with a fresh database (maybe will change later)
            , npSystemStart  = startTime
            , npSecretKey    = genesisSecretKeys !! nodeNumber
            , npVssKeyPair   = genesisVssKeyPairs !! nodeNumber
            , npBaseParams   = baseParams
            }

    runNodeStats params

main :: IO ()
main = do
    RBO {..} <- execParser parseOptions
    case nodeSpecificOptions of
        FullNodeOptions {..} -> startFullNode configFilePath nodeNumber
        _                    -> startSupporter configFilePath
