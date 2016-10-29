{-# LANGUAGE DeriveGeneric #-}

-- | Types used for stats requests

module Pos.Communication.Types.Statistics
       ( RequestStat (..)
       , ResponseStat (..)
       ) where

import           Data.Binary               (Binary)
import           Data.MessagePack          (MessagePack)
import           Universum

import           Control.TimeWarp.Rpc      (Message (..))

import           Pos.Statistics.MonadStats (CounterLabel)
import           Pos.Types                 (Timestamp (..))

-- | Message: someone requested a stat
data RequestStat =
    RequestStat !CounterLabel
    deriving (Generic)

-- | Message: send the list with stats back
data ResponseStat a =
    ResponseStat !(Maybe [a])
    deriving (Generic)

instance Binary RequestStat
instance Binary a => Binary (ResponseStat a)

instance MessagePack RequestStat
instance MessagePack a => MessagePack (ResponseStat a)

instance Message RequestStat where
    messageName _ = "RequestStat"

instance (Binary a, Typeable a) => Message (ResponseStat a) where
    messageName _ = "ResponseStat"
