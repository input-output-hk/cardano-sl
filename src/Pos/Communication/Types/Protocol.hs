-- | Protocol/versioning related communication types.

module Pos.Communication.Types.Protocol
       ( VersionReq (..)
       , VersionResp (..)
       ) where

import           Control.TimeWarp.Rpc (Message (..), messageName')
import           Universum

import           Pos.Types            (ProtocolVersion)

-- | Version request message. 'VersionResp' is expected as response.
data VersionReq = VersionReq
    deriving (Generic)

instance Message VersionReq where
    messageName _ = "VersionReq"
    formatMessage = messageName'

-- | Version response (on 'VersionReq' response).
data VersionResp = VersionResp
    { vRespMagic           :: Int32
    , vRespProtocolVersion :: ProtocolVersion
    } deriving (Generic)

instance Message VersionResp where
    messageName _ = "VersionResp"
    formatMessage = messageName'
