{-# LANGUAGE ScopedTypeVariables #-}

-- | Command line options of pos-node.

module SenderOptions
       ( Args (..)
       , argsParser
       ) where

import           Data.Int (Int64)
import           Data.Monoid ((<>))
import           Data.String (fromString)
import           Data.Word (Word16)
import           Options.Applicative.Simple (Parser, auto, help, long, metavar, option, optional,
                                             short, showDefault, some, strOption, value, readerError)
import           Pos.Util.OptParse (fromParsec)
import           Serokell.Util.Parse (connection)

data Args = Args
    { logConfig    :: !(Maybe FilePath)
    , logsPrefix   :: !(Maybe FilePath)
    , recipients   :: ![(String, Word16)]
    , threadNum    :: !Int
    , msgNum       :: !Int
    , msgRate      :: !Word
    , duration     :: !Int
    , payloadBound :: !Int64
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
    some
        (option recipient $
         long "peer" <> metavar "HOST:PORT" <> help "Recipient's ip:port")
    <*>
    option
        auto
        (short 't'
          <> long "thread-num"
          <> metavar "INTEGER"
          <> value 5
          <> showDefault <>
         help "Number of threads to use")
    <*>
    option
        auto
        (short 'm'
          <> long "msg-num"
          <> metavar "INTEGER"
          <> value 1000
          <> showDefault <>
         help "Number of messages to send")
    <*>
    option
        auto
        (   short 'r'
         <> long "msg-rate"
         <> metavar "INTEGER"
         <> value (-1)
         <> help "Number of messages to send per second (default: ∞)")
    <*>
    option
        auto
        (short 'd'
          <> long "duration"
          <> metavar "INTEGER"
          <> value 10
          <> showDefault <>
         help "Time to run before stopping, seconds")
     <*>
    option
        auto
        (short 'p'
          <> long "payload"
          <> metavar "INTEGER"
          <> value 0
          <> showDefault <>
         help "Defines upper bound on sent message size")
  where
    recipient = fromParsec connection >>= \(h, mp) ->
        case mp of
            Just p -> return (fromString h, p)
            _      -> readerError $ "No port specified for host " <> h
