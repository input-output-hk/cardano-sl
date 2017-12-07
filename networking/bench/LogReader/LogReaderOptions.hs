{-# LANGUAGE ScopedTypeVariables #-}

-- | Command line options of pos-node.

module LogReaderOptions
       ( Args (..)
       , argsParser
       ) where

import           Data.Monoid                ((<>))
import           Options.Applicative.Simple (Parser, help, long, metavar, short,
                                             showDefault, some, strOption, value)

data Args = Args
    { inputFiles :: ![FilePath]
    , resultFile :: !FilePath
    } deriving Show

argsParser :: Parser Args
argsParser =
    Args <$>
    some
        (strOption $
            short 'i'
              <> long "input"
              <> metavar "FILEPATH" <>
             help "Files to get data from")
    <*>
    strOption
        (short 'o'
          <> long "output"
          <> metavar "FILEPATH"
          <> value "measures.csv"
          <> showDefault <>
         help "File to write results in csv form")
