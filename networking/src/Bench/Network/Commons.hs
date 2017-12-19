{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Bench.Network.Commons
       ( MsgId
       , Ping (..)
       , Pong (..)
       , Payload (..)
       , logMeasure

       , loadLogConfig

       , Timestamp
       , MeasureEvent (..)
       , MeasureInfo (..)
       , LogMessage (..)
       , measureInfoParser
       , logMessageParser
       ) where

import           Control.Applicative ((<|>))
import           Control.Lens (zoom, (?=))
import           Control.Monad (join)
import           Control.Monad.Trans (MonadIO (..))

import           Data.Attoparsec.Text (Parser, char, decimal, string, takeWhile)
import           Data.Binary (Binary)
import           Data.Binary (Binary (..))
import qualified Data.ByteString.Lazy as BL
import           Data.Data (Data)
import           Data.Functor (($>))
import           Data.Int (Int64)
import           Data.Monoid ((<>))
import           Data.Text.Buildable (Buildable (build))
import           Data.Time.Units (toMicroseconds)

import qualified Formatting as F
import           GHC.Generics (Generic)
import           Prelude hiding (takeWhile)
import           System.Wlog (LoggerConfig (..), WithLogger, errorPlus, fromScratch, infoPlus,
                              lcTree, logInfo, ltSeverity, maybeLogsDirB, parseLoggerConfig,
                              productionB, setupLogging, warningPlus, zoomLogger)

import           Mockable.CurrentTime (realTime)
import           Node (Message (..))

-- * Transfered data types

type MsgId = Int

-- | Serializes into message of (given size + const)
data Payload = Payload
    { getPayload :: Int64
    } deriving (Generic, Data)

data Ping = Ping MsgId Payload
    deriving (Generic, Data, Binary)

instance Message Ping where
    messageCode _ = 0
    formatMessage _ = "Ping"

data Pong = Pong MsgId Payload
    deriving (Generic, Data, Binary)

instance Message Pong where
    messageCode _ = 1
    formatMessage _ = "Pong"

instance Binary Payload where
    get = Payload . BL.length <$> get
    put (Payload l) = put $ BL.replicate l 42


-- * Util

logMeasure :: (MonadIO m, WithLogger m) => MeasureEvent -> MsgId -> Payload -> m ()
logMeasure miEvent miId miPayload = do
    miTime <- toMicroseconds <$> realTime
    logInfo $ F.sformat F.build $ LogMessage MeasureInfo{..}

defaultLogConfig :: LoggerConfig
defaultLogConfig = fromScratch $ zoom lcTree $ do
    ltSeverity ?= warningPlus
    zoomLogger "sender" $ do
        ltSeverity ?= infoPlus
        commLogger
    zoomLogger "receiver" $ do
        ltSeverity ?= infoPlus
        commLogger
  where
    commLogger = zoomLogger "comm" $ ltSeverity ?= errorPlus

loadLogConfig :: MonadIO m => Maybe FilePath -> Maybe FilePath -> m ()
loadLogConfig logsPrefix configFile = do
    let cfgBuilder = productionB <> maybeLogsDirB logsPrefix
    loggerConfig <- maybe (return defaultLogConfig) parseLoggerConfig configFile
    setupLogging Nothing $ loggerConfig <> cfgBuilder


-- * Logging & parsing

-- ** Measure event

-- | Type of event in measurement.
data MeasureEvent
    = PingSent
    | PingReceived
    | PongSent
    | PongReceived
    deriving (Show, Eq, Ord, Enum, Bounded)

instance Buildable MeasureEvent where
    build PingSent     = "• → "
    build PingReceived = " → •"
    build PongSent     = " ← •"
    build PongReceived = "• ← "

measureEventParser :: Parser MeasureEvent
measureEventParser = string "• → " $> PingSent
                 <|> string " → •" $> PingReceived
                 <|> string " ← •" $> PongSent
                 <|> string "• ← " $> PongReceived


-- ** Measure info

type Timestamp = Integer

-- | Single event in measurement.
data MeasureInfo = MeasureInfo
    { miId      :: MsgId
    , miEvent   :: MeasureEvent
    , miTime    :: Timestamp
    , miPayload :: Payload
    }

instance Buildable MeasureInfo where
    build MeasureInfo{..} = mconcat
        [ build miId
        , " "
        , build miEvent
        , " ("
        , build $ getPayload miPayload
        , ") "
        , build miTime
        ]

measureInfoParser :: Parser MeasureInfo
measureInfoParser = do
    miId <- decimal
    _ <- string " "
    miEvent <- measureEventParser
    _ <- string " ("
    miPayload <- Payload <$> decimal
    _ <- string ") "
    miTime <- decimal
    return MeasureInfo{..}


-- ** Log message

-- | Allows to extract bare message content from logs.
-- Just inserts separator at beginning.
data LogMessage a = LogMessage a

instance Buildable a => Buildable (LogMessage a) where
    build (LogMessage a) = "#" <> build a

logMessageParser :: Parser a -> Parser (Maybe (LogMessage a))
logMessageParser p = (takeWhile (/= '#') >>) . join $ do
        (char '#' *> pure (Just . LogMessage <$> p))
    <|> pure (pure Nothing)
