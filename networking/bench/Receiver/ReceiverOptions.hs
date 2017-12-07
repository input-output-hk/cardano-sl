{-# LANGUAGE ScopedTypeVariables #-}

-- | Command line options of pos-node.

module ReceiverOptions
       ( Args (..)
       , argsParser
       ) where

import           Data.Monoid ((<>))
import           Data.Word (Word16)
import           Options.Applicative.Simple (Parser, auto, help, long, metavar, option, optional,
                                             short, showDefault, strOption, switch, value)

data Args = Args
    { logConfig  :: !(Maybe FilePath)
    , logsPrefix :: !(Maybe FilePath)
    , port       :: !Word16
    , noPong     :: !Bool
    , duration   :: !Int
    }
  deriving Show

--TODO introduce subcommands
argsParser :: Parser Args
argsParser =
    Args <$>
    optional
        (strOption
            (long "log-config"
              <> metavar "FILEPATH" <>
             help "Path to logger configuration")
        )
    <*>
    optional
        (strOption $
         long "logs-prefix" <> metavar "FILEPATH" <> help "Prefix to logger output path")
    <*>
    option
        auto
        (short 'p'
          <> long "port"
          <> metavar "INTEGER"
          <> value 3000
          <> showDefault <>
         help "Port to work on")
    <*>
    switch
        (long "no-pong" <>
         help "Don't send pong")
    <*>
    option
        auto
        (short 'd'
          <> long "duration"
          <> metavar "INTEGER"
          <> value 10
          <> showDefault <>
         help "Time to run before stopping, seconds")
