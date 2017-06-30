{-# LANGUAGE ApplicativeDo #-}

module Pos.Statistics.Statsd
       ( StatsdParams (..)
       , statsdParamsOption
       ) where

import           Universum
import qualified Options.Applicative.Simple as Opt
import           Serokell.Util.OptParse     (fromParsec)
import           Pos.Util.TimeWarp          (NetworkAddress, addrParserNoWildcard)

data StatsdParams = StatsdParams
    { statsdHost :: !Text
    , statsdPort :: !Int
    , statsdInterval :: !Int
    , statsdDebug :: !Bool
    , statsdPrefix :: !Text
    , statsdSuffix :: !Text
    } deriving (Show)

statsdParamsOption :: Opt.Parser StatsdParams
statsdParamsOption = do
    addr <- statsdServerOption
    interval <- statsdIntervalOption
    debug <- statsdDebugOption
    prefix <- statsdPrefixOption
    suffix <- statsdSuffixOption
    pure $ StatsdParams
        { -- The network address parser only accepts ByteStrings which are
          -- UTF8 encoded
          statsdHost = decodeUtf8 (fst addr)
        , statsdPort = fromIntegral (snd addr)
        , statsdInterval = interval
        , statsdDebug = debug
        , statsdPrefix = prefix
        , statsdSuffix = suffix
        }

statsdServerOption :: Opt.Parser NetworkAddress
statsdServerOption = Opt.option (fromParsec addrParserNoWildcard) $
    Opt.long "statsd-server" <>
    Opt.metavar "IP:PORT" <>
    Opt.help "Host and port for the statsd server"

statsdIntervalOption :: Opt.Parser Int
statsdIntervalOption = Opt.option Opt.auto $
    Opt.long "statsd-interval" <>
    Opt.value 1000 <>
    Opt.metavar "MILLISECONDS" <>
    Opt.help "Polling interval for statsd (milliseconds)"

statsdDebugOption :: Opt.Parser Bool
statsdDebugOption = Opt.option Opt.auto $
    Opt.long "statsd-debug" <>
    Opt.value False <>
    Opt.metavar "BOOL" <>
    Opt.help "Enable statsd debug mode"

statsdPrefixOption :: Opt.Parser Text
statsdPrefixOption = Opt.option Opt.auto $
    Opt.long "statsd-prefix" <>
    Opt.value "" <>
    Opt.metavar "TEXT" <>
    Opt.help "Prefix for statsd"

statsdSuffixOption :: Opt.Parser Text
statsdSuffixOption = Opt.option Opt.auto $
    Opt.long "statsd-suffix" <>
    Opt.value "" <>
    Opt.metavar "TEXT" <>
    Opt.help "Suffix for statsd"
