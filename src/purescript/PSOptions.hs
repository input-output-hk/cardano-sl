-- | Command line options of purescript generator.

module PSOptions
       ( Args (..)
       , getPSOptions
       ) where

import           Data.Version               (showVersion)
import           Options.Applicative.Simple (Parser, auto, help, long, metavar, option,
                                             showDefault, simpleOptions, strOption,
                                             switch, value)
import           Prelude                    (show)
import           Universum                  hiding (show)

import           Paths_cardano_sl_explorer  (version)

data Args = Args
    { bridgePath :: !FilePath
    }
  deriving Show

argsParser :: Parser Args
argsParser =
    Args <$>
    strOption
        (long "bridge-path" <> metavar "FILEPATH" <> value "Generated" <>
        help "Path where to dump generated modules")

getPSOptions :: IO Args
getPSOptions = do
    (res, ()) <-
        simpleOptions
            ("cardano-explorer-hs2purs-" <> showVersion version)
            "CardanoSL explorer ps datatypes generator"
            "CardanoSL explorer ps datatypes generator."
            argsParser
            empty
    return res
