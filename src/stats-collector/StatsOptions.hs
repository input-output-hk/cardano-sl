-- | Options for statistic collector

module StatsOptions
    ( StatOpts (..)
    , readOptions
    ) where

import           Control.Applicative        (empty)
import           Data.Monoid                ((<>))
import qualified Data.Text                  as T
import           Options.Applicative.Simple (Parser, auto, help, long, metavar, option,
                                             showDefault, simpleOptions, strOption, value)
import           Universum                  hiding ((<>))


data StatOpts = StatOpts
    { soOutputDir    :: FilePath
    , soOutputPrefix :: Text
    , soConfigPath   :: FilePath
    , soTpsStep      :: Int
    , soTpsMax       :: Int
    , soSshPassword  :: Text
    } deriving (Show)

argsParser :: Parser StatOpts
argsParser =
    StatOpts <$>
    strOption
        (long "output-dir" <> metavar "FILEPATH" <> value "." <>
         help "Path to the output directory") <*>
    (T.pack <$>
     strOption
        (long "output-pref" <> metavar "STRING" <> value "stats-" <>
         help "Prefix for statistic dir output")) <*>
    strOption
        (long "config" <> metavar "FILEPATH" <> value "stats.yaml" <>
         help "Path to the configuration file (nodes list)") <*>
    option
        auto
        (long "tps-step" <> metavar "INTEGER" <> value 10 <> showDefault <>
         help "Step of TPS") <*>
    option
        auto
        (long "tps-max" <> metavar "INTEGER" <> value 50 <> showDefault <>
         help "Max value of TPS") <*>
    (T.pack <$> strOption
        (long "ssh-passwd" <> metavar "STRING" <>
         help "Password for ssh node instances"))

readOptions :: IO StatOpts
readOptions = do
    (res,()) <- simpleOptions
        "stats-collector"
        "Statistics collecting service"
        ""
        argsParser
        empty
    pure res
