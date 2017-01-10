-- | Types used for communication about system start.

module Pos.Communication.Types.SysStart
       ( SysStartRequest (..)
       , SysStartResponse (..)
       ) where

import           Universum

import           Message.Message (Message (..))
import           Pos.Types       (Timestamp)

-- | Communication request for system start.
data SysStartRequest = SysStartRequest
    deriving (Show, Eq, Generic)

-- | Response to 'SysStartRequest'.
data SysStartResponse = SysStartResponse !Timestamp
    deriving (Show, Eq, Generic)

instance Message SysStartRequest where
    messageName _ = "SysStartRequest"
    formatMessage _ = "SysStartRequest"

instance Message SysStartResponse where
    messageName _ = "SysStartResponse"
    formatMessage _ = "SysStartResponse"
