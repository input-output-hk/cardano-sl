module Options
    ( Options (..)
    , parseOptions
    ) where

import Options.Applicative

import Universum

data Options = Options
    { times   :: FilePath
    , graph   :: FilePath
    , logDirs :: [FilePath]
    } deriving Show

options :: Parser Options
options = Options <$> strOption 
                        (  long "times"
                        <> short 't'
                        <> metavar "TIMES"
                        <> help "target for the times file"
                        )
                  <*> strOption 
                        (  long "graph"
                        <> short 'g'
                        <> metavar "GRAPH"
                        <> help "template for the graph png files"
                        )
                  <*> (some (argument str 
                        (  metavar "LOGDIRS..."
                        <> help "directories containing the log files"
                        )))

parseOptions :: IO Options
parseOptions = execParser $ info (options <**> helper)
    (  fullDesc
    <> progDesc "Analyzes the json logs from LOGDIRS... and writes the analysis to TIMES and the files given by GRAPH."
    <> header "cardano-post-mortem"
    )
