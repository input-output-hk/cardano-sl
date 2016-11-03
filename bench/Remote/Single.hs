module Main (main) where

import           Data.Monoid               ((<>))
import           Options.Applicative       (Parser, ParserInfo, auto, command, execParser,
                                            fullDesc, header, help, helper, info, long,
                                            metavar, option, progDesc, short, strOption,
                                            subparser, switch, value)
import           Universum                 hiding ((<>))

import           Bench.Pos.Remote.Launcher (NodeNumber, startFullNode, startSupporter)

data RemoteBenchOptions = RBO
    { nodeSpecificOptions :: NodeSpecificOptions
    , configFilePath      :: FilePath
    }


data NodeSpecificOptions = FullNodeOptions { nodeNumber :: NodeNumber, isTimeLord :: Bool }
                         | SupporterOptions

fullNodeParser :: Parser NodeSpecificOptions
fullNodeParser = FullNodeOptions
    <$> option auto (long "node-index"
                  <> short 'n'
                  <> metavar "INDEX"
                  <> help "This node index in the list of nodes (must be in range 0..41)")
    <*> switch (long "time-lord"
             <> short 't'
             <> help "Mark this node as time lord")

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
    <> header   "pos-bench-singlr - run single node for benchmarking remotely"
    <> progDesc "Runs PoS node with statlogs enabled"

main :: IO ()
main = do
    RBO {..} <- execParser parseOptions
    case nodeSpecificOptions of
        FullNodeOptions {..} -> startFullNode configFilePath nodeNumber isTimeLord
        _                    -> startSupporter configFilePath
