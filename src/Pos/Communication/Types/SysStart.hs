-- | Types used for communication about system start.

module Pos.Communication.Types.SysStart
       ( SysStartRequest (..)
       , SysStartResponse (..)
       ) where

import           Universum

import           Pos.Types (Timestamp)

-- | Communication request for system start.
data SysStartRequest = SysStartRequest
    deriving (Show, Eq, Generic)

-- | Response to 'SysStartRequest'.
data SysStartResponse = SysStartResponse !Timestamp
    deriving (Show, Eq, Generic)
