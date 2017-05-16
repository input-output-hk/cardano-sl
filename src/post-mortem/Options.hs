module Options
    ( Options (..)
    , parseOptions
    ) where

import Options.Applicative

import Universum

data Options = Options
    { logDir   :: FilePath
    , timesSVG :: FilePath
    , graphPNG :: FilePath
    } deriving Show

options :: Parser Options
options = Options <$> strOption (  long "logdir"
                                <> short 'd'
                                <> metavar "LOGDIR"
                                <> help "directory containing the log files"
                                )
                  <*> strOption (  long "times"
                                <> short 't'
                                <> metavar "TIMES"
                                <> help "target for the times svg file"
                                )
                  <*> strOption (  long "graph"
                                <> short 'g'
                                <> metavar "GRAPH"
                                <> help "target for the graph png file"
                                )

parseOptions :: IO Options
parseOptions = execParser $ info (options <**> helper)
    (  fullDesc
    <> progDesc "Analyzes the json logs from LOGDIR and writes the analysis to TIMES and GRAPH."
    <> header "cardano-post-mortem"
    )
