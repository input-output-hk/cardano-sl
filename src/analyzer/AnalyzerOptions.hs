{-# LANGUAGE ScopedTypeVariables #-}

-- | Command line options of pos-node.

module AnalyzerOptions
       ( Args (..)
       , argsParser
       ) where

import           Data.Monoid                ((<>))
import           Options.Applicative.Simple (Parser, auto, help, long, many, metavar,
                                             option, showDefault, strOption, switch,
                                             value)
import           Universum                  hiding ((<>))

data Args = Args
    { files :: ![FilePath]
    }
  deriving Show

--TODO introduce subcommands
argsParser :: Parser Args
argsParser =
    Args <$>
    many
    (strOption
        (long "file" <> metavar "FILEPATH" <>
         help "JSON file to analyze"))
