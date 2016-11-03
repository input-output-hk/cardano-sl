-- | Executable for collecting stats data from nodes

import           Data.Monoid               ((<>))
import qualified Options.Applicative       as Opts
import           Universum                 hiding ((<>))

import           Bench.Pos.Remote.Launcher (startCollector)

data CollectorOptions = CO
  { coConfigPath :: FilePath
  }

optParser :: Opts.Parser CollectorOptions
optParser = CO
  <$> Opts.strOption (Opts.long "config"
                   <> Opts.short 'c'
                   <> Opts.metavar "PATH_TO_CONFIG"
                   <> Opts.value "collector.yaml"
                   <> Opts.help "Path to YAML config file")

parseOptions :: Opts.ParserInfo CollectorOptions
parseOptions = Opts.info (Opts.helper <*> optParser) $ Opts.fullDesc
    <> Opts.header   "pos-bench-remote - distributed benchmarks for Cardano PoS"
    <> Opts.progDesc "Runs PoS full node and starts benchmarking transactions"

main :: IO ()
main = do
  CO {..} <- Opts.execParser parseOptions
  startCollector coConfigPath
