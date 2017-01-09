-- | Protocol/versioning related communication types.

module Pos.Communication.Types.Protocol
       ( VersionReq (..)
       , VersionResp (..)
       ) where

import           Control.TimeWarp.Rpc (Message (..), messageName')
import           Universum

import           Pos.Types            (ProtocolVersion)
import qualified Message.Message       as M
import qualified Data.ByteString.Char8 as BC

-- | Version request message. 'VersionResp' is expected as response.
data VersionReq = VersionReq
    deriving (Show,Generic)

instance Message VersionReq where
    messageName _ = "VersionReq"
    formatMessage = messageName'

instance M.Message VersionReq where
    messageName _ = M.MessageName $ BC.pack "VersionReq"
    formatMessage _ = "VersionReq"

-- | Version response (on 'VersionReq' response).
data VersionResp = VersionResp
    { vRespMagic           :: Int32
    , vRespProtocolVersion :: ProtocolVersion
    } deriving (Show,Generic)

instance Message VersionResp where
    messageName _ = "VersionResp"
    formatMessage = messageName'

instance M.Message VersionResp where
    messageName _ = M.MessageName $ BC.pack "VersionResp"
    formatMessage _ = "VersionResp"
