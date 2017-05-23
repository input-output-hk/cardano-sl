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
    | TxRelay ![FilePath]

overviewOptions :: Parser Options
overviewOptions = Overview <$> (some (argument str 
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

txRelayOptions :: Parser Options
txRelayOptions = TxRelay <$> (some (argument str 
    (  metavar "LOGDIRS..."
    <> help "directories containing the log files"
    )))

options :: Parser Options
options = hsubparser
    (  command "overview" (info overviewOptions (progDesc "analyzes the json logs from LOGDIRS..."))
    <> command "focus"    (info focusedOptions  (progDesc "analyzes transaction FOCUS in log folder LOGDIR"))
    <> command "txrelay"  (info txRelayOptions  (progDesc "analyzes transaction relays in the json logs from LOGDIRS..."))
    )

parseOptions :: IO Options
parseOptions = execParser $ info (options <**> helper)
    (  fullDesc
    <> progDesc "analyzes the json logs from several directories or focusses on a single transaction"
    <> header "cardano-post-mortem"
    )
