{-# LANGUAGE DeriveGeneric #-}
-- | Types used for communication about system start.

module Pos.Communication.Types.SysStart
       ( SysStartRequest (..)
       , SysStartResponse (..)
       ) where

import           Data.Binary          (Binary)
import           Data.MessagePack     (MessagePack)
import           Universum

import           Control.TimeWarp.Rpc (Message (..), MessageName)
import           Pos.Types            (Timestamp)

data SysStartRequest = SysStartRequest
    deriving (Generic)

data SysStartResponse = SysStartResponse !(Maybe Timestamp)
    deriving (Generic)

instance Binary SysStartRequest
instance Binary SysStartResponse

instance MessagePack SysStartRequest
instance MessagePack SysStartResponse

instance Message SysStartRequest where
    messageName _ = "SysStartRequest"

instance Message SysStartResponse where
    messageName _ = "SysStartResponse"
