module Options
    ( Options (..)
    , parseOptions
    ) where

import Options.Applicative

import Types
import Universum

data Options =
      Overview ![FilePath]
    | Focus !TxHash !FilePath

overViewOptions :: Parser Options
overViewOptions = Overview <$> (some (argument str 
    (  metavar "LOGDIRS..."
    <> help "directories containing the log files"
    )))

focusedOptions :: Parser Options
focusedOptions = Focus <$> (toText <$> argument str
                                (  metavar "FOCUS"
                                <> help "transaction hash to focus on"
                                ))
                       <*> (argument str
                                (  metavar "LOGDIR"
                                <> help "directoy containing the logfiles"
                                ))

options :: Parser Options
options = hsubparser
    (  command "overview" (info overViewOptions (progDesc "analyzes the json logs from LOGDIRS..."))
    <> command "focus"    (info focusedOptions  (progDesc "analyzes transaction FOCUS in log folder LOGDIR"))
    )

parseOptions :: IO Options
parseOptions = execParser $ info (options <**> helper)
    (  fullDesc
    <> progDesc "analyzes the json logs from several directories or focusses on a single transaction"
    <> header "cardano-post-mortem"
    )
