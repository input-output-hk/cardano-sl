-- | Protocol/versioning related communication types.

module Pos.Communication.Types.Protocol
       ( VersionReq (..)
       , VersionResp (..)
       ) where

import           Node.Message          (Message (..), MessageName (..))
import           Universum

import qualified Data.ByteString.Char8 as BC
import           Pos.Types             (BlockVersion)

-- | Version request message. 'VersionResp' is expected as response.
data VersionReq = VersionReq
    deriving (Show,Generic)

instance Message VersionReq where
    messageName _ = MessageName $ BC.pack "VersionReq"
    formatMessage _ = "VersionReq"

-- | Version response (on 'VersionReq' response).
data VersionResp = VersionResp
    { vRespMagic        :: Int32
    , vRespBlockVersion :: BlockVersion
    } deriving (Show,Generic)

instance Message VersionResp where
    messageName _ = MessageName $ BC.pack "VersionResp"
    formatMessage _ = "VersionResp"
