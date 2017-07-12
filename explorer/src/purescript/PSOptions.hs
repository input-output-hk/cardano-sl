-- | Command line options of purescript generator.

module PSOptions
       ( Args (..)
       , getPSOptions
       ) where

import           Data.Version               (showVersion)
import           Options.Applicative.Simple (Parser, help, long, metavar,
                                             simpleOptions, strOption, value)
import           Universum                  hiding (show)

import           Paths_cardano_sl_explorer  (version)

newtype Args = Args
    { bridgePath :: FilePath
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
