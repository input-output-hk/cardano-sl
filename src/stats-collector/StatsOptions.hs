-- | Options for statistic collector

module StatsOptions
    ( StatOpts (..)
    , readOptions
    ) where

import           Control.Applicative        (empty)
import           Data.Monoid                ((<>))
import qualified Data.Text                  as T
import           Options.Applicative.Simple (Parser, auto, help, long, metavar, option,
                                             showDefault, simpleOptions, strOption,
                                             switch, value)
import           Universum                  hiding ((<>))


data StatOpts = StatOpts
    { soOutputDir    :: FilePath
    , soOutputPrefix :: [Char]
    , soInterval     :: Integer -- seconds
    , soLoop         :: Bool
    , soConfigPath   :: FilePath
    , soSshPassword  :: Text
    } deriving (Show)

argsParser :: Parser StatOpts
argsParser =
    StatOpts <$>
    strOption
        (long "output-dir" <> metavar "FILEPATH" <> value "." <>
         showDefault <> help "Path to the output directory") <*>
    (strOption
        (long "output-pref" <> metavar "STRING" <> value "stats-node-" <>
         showDefault <> help "Prefix for statistic dir output")) <*>
    option auto (long "interval" <> metavar "SECONDS" <>
                 help "Interval to collect data since. In loop mode -- delay." <>
                 value 90 <> showDefault) <*>
    switch (long "loop" <> help "Loop collecting (every interval, from start)") <*>
    strOption
        (long "config" <> metavar "FILEPATH" <> value "stats.yaml" <>
         help "Path to the configuration file (nodes list)") <*>
    (T.pack <$> strOption
        (long "ssh-passwd" <> metavar "STRING" <>
         help "Password for ssh node instances"))

readOptions :: IO StatOpts
readOptions = do
    (res,()) <- simpleOptions
        "Check .cabal version"
        "Statistics collecting & benchmarking service for cardano sl"
        ""
        argsParser
        empty
    pure res
