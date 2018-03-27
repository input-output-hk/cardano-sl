{-# LANGUAGE OverloadedStrings #-}

module Bench.Pos.Diffusion.Download.Common
    ( protocolMagic
    , protocolConstants
    , blockVersion
    , bracketTransport
    , CommonOptions (..)
    , parseCommonOptions
    , bunchOfHashes
    , generateBlock
    , enableLogging
    ) where

import           Universum

import           Control.Exception (SomeException, throwIO)
import           Data.Functor.Contravariant (contramap)
import           Data.Time.Units (Microsecond)
import qualified Data.ByteString.Lazy as LBS
import           Network.Transport (Transport)
import qualified Network.Transport.TCP as TCP
import           Options.Applicative
import qualified System.Wlog as Wlog

import           Pos.Arbitrary.Block.Generate (generateMainBlock)
import           Pos.Crypto (ProtocolMagic (..))
import           Pos.Crypto.Hashing (unsafeMkAbstractHash)
import           Pos.Core (Block, BlockVersion (..), VssMinTTL (..), VssMaxTTL (..),
                           ProtocolConstants (..), HeaderHash)
import           Pos.Diffusion.Transport.TCP (bracketTransportTCP)
import           Pos.Util.Trace (Trace, Severity (Error), wlogTrace)

-- | Switch on the log-warper global logger thing.
-- In my experience this just makes logging go, in general.
enableLogging :: IO ()
enableLogging = Wlog.setupLogging Nothing $ (Wlog.defaultConfig "arbitrary_logger_name")
    { Wlog._lcTree = Wlog.LoggerTree mempty [] (Just Wlog.allSeverities)
    }

bunchOfHashes :: [HeaderHash]
bunchOfHashes = unsafeMkAbstractHash . LBS.pack . pure <$> [minBound..maxBound]

protocolMagic :: ProtocolMagic
protocolMagic = ProtocolMagic 0

protocolConstants :: ProtocolConstants
protocolConstants = ProtocolConstants
    { pcK = 2160
    , pcVssMinTTL = VssMinTTL 2
    , pcVssMaxTTL = VssMaxTTL 6
    }

blockVersion :: BlockVersion
blockVersion = BlockVersion
    { bvMajor = 0
    , bvMinor = 1
    , bvAlt = 0
    }

generateBlock :: Int -> Int -> Block
generateBlock seed size = Right $ generateMainBlock protocolMagic protocolConstants seed size

bracketTransport :: Microsecond -> Maybe String -> Maybe Word16 -> (Transport -> IO t) -> IO t
bracketTransport timeout mHost mPort = bracketTransportTCP logTrace timeout tcpAddr
  where
    logTrace :: Trace IO Text
    logTrace = contramap ((,) Error) (wlogTrace "transport")
    tcpAddr = case mHost of
        Nothing -> TCP.Unaddressable
        Just host -> TCP.Addressable tcpAddrInfo
          where
            tcpAddrInfo = case mPort of
                Just port -> TCP.TCPAddrInfo host (Prelude.show port) (const (host, Prelude.show port))
                Nothing   -> TCP.TCPAddrInfo host "0" ((,) host)

data CommonOptions = CommonOptions
    { coHost         :: Maybe String
    , coPort         :: Maybe Word16
    , coDefaultPort  :: Word16
    , coBatchSize    :: Word
    , coStreamWindow :: Word32
    , coShowLogs     :: Bool
    }

parseCommonOptions :: Parser CommonOptions
parseCommonOptions =
    CommonOptions <$> parseNetworkHost
                  <*> parseNetworkPort
                  <*> parseDefaultPort
                  <*> parseBatchSize
                  <*> parseStreamWindow
                  <*> parseShowLogs

parseNetworkHost :: Parser (Maybe String)
parseNetworkHost = optional $ strOption (long "host" <> short 'h')

parseNetworkPort :: Parser (Maybe Word16)
parseNetworkPort = optional $ option auto (long "port" <> short 'p')

parseDefaultPort :: Parser Word16
parseDefaultPort = option auto (long "default-port" <> value 3000)

parseBatchSize :: Parser Word
parseBatchSize = option auto (long "batch-size" <> short 'b' <> value 2200)

parseStreamWindow :: Parser Word32
parseStreamWindow = option auto (long "stream-window" <> short 's' <> value 4400)

parseShowLogs :: Parser Bool
parseShowLogs = switch (long "show-logs" <> short 'l')
