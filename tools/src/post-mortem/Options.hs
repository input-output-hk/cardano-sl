module Options
    ( Options (..)
    , parseOptions
    ) where

import           Options.Applicative

import           Types
import           Universum

data Options =
      Overview !Double ![FilePath]
    | Focus !TxHash !FilePath
    | TxRelay ![FilePath]
    | Throughput !Double !Double ![FilePath]

overviewOptions :: Parser Options
overviewOptions = Overview <$> (toProb <$> argument auto
                                (  metavar "SAMPLEPROB"
                                <> help "sample probability for frequent events"
                                ))
                           <*> (some (argument str
                                (  metavar "LOGDIRS..."
                                <> help "directories containing the log files"
                                )))

focusedOptions :: Parser Options
focusedOptions = Focus <$> ((toText :: String -> Text) <$> argument str
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

throughputOptions :: Parser Options
throughputOptions = Throughput <$> (argument auto
                                    (  metavar "TXWINDOW"
                                    <> help "time window for transaction throughput (in seconds)"
                                    ))
                               <*> (argument auto
                                    (  metavar "WAITWINDOW"
                                    <> help "time window for waiting times (in seconds)"
                                    ))
                               <*> (some (argument str
                                    (  metavar "LOGDIRS..."
                                    <> help "directories containing the log files"
                                    )))

options :: Parser Options
options = hsubparser
    (  command "overview"   (info overviewOptions   (progDesc "analyzes the json logs from LOGDIRS..."))
    <> command "focus"      (info focusedOptions    (progDesc "analyzes transaction FOCUS in log folder LOGDIR"))
    <> command "txrelay"    (info txRelayOptions    (progDesc "analyzes transaction relays in the json logs from LOGDIRS..."))
    <> command "throughput" (info throughputOptions (progDesc "analyzes transaction throughput and waiting time per time windows TXWINDOW and WAITWINDOW in the json logs from LOGDIRS..."))
    )

parseOptions :: IO Options
parseOptions = execParser $ info (options <**> helper)
    (  fullDesc
    <> progDesc "analyzes the json logs from several directories or focuses on a single transaction"
    <> header "cardano-post-mortem"
    )

toProb :: Double -> Double
toProb = min 1 . max 0
