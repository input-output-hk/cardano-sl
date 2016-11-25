{-# LANGUAGE DeriveGeneric #-}
-- | Types used for communication about system start.

module Pos.Communication.Types.SysStart
       ( SysStartRequest (..)
       , SysStartResponse (..)
       ) where

import           Data.Binary          (Binary)
import           Data.MessagePack     (MessagePack)
import           Universum

import           Control.TimeWarp.Rpc (Message (..), messageName')
import           Pos.Types            (SlotId, Timestamp)

-- | Communication request for system start.
data SysStartRequest = SysStartRequest
    deriving (Generic)

-- | Response to 'SysStartRequest'.
data SysStartResponse = SysStartResponse !Timestamp !(Maybe SlotId)
    deriving (Generic)

instance Binary SysStartRequest
instance Binary SysStartResponse

instance MessagePack SysStartRequest

instance MessagePack SysStartResponse

instance Message SysStartRequest where
    messageName _ = "SysStartRequest"
    formatMessage = messageName'

instance Message SysStartResponse where
    messageName _ = "SysStartResponse"
    formatMessage = messageName'
