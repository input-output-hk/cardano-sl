module Options
    ( Options (..)
    , parseOptions
    ) where

import Options.Applicative

import Universum

data Options = Options
    { logDirs :: [FilePath]
    } deriving Show

options :: Parser Options
options = Options <$> (some (argument str 
                          (  metavar "LOGDIRS..."
                          <> help "directories containing the log files"
                          )))

parseOptions :: IO Options
parseOptions = execParser $ info (options <**> helper)
    (  fullDesc
    <> progDesc "Analyzes the json logs from LOGDIRS... ."
    <> header "cardano-post-mortem"
    )
