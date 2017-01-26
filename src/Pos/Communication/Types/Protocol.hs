-- | Protocol/versioning related communication types.

module Pos.Communication.Types.Protocol
       (
       ) where

import           Node.Message (MessageName)
import           Universum

import           Pos.Types    (BlockVersion)

-- | Version request message. 'VersionResp' is expected as response.
data VersionReq = VersionReq
    deriving (Show,Generic)

-- | Version response (on 'VersionReq' response).
data VersionResp = VersionResp
    { vRespMagic        :: Int32
    , vRespBlockVersion :: BlockVersion
    } deriving (Show,Generic)

data HandlerSpec
  = ConvHandler
        { hsRecvType :: MessageName
        , hsSendType :: MessageName
        }
  | OneMsgHandler
        { hsMsgType :: MessageName
        }
    deriving (Show, Generic)

data VerInfo = VerInfo
    { vIMagic        :: Int32
    , vIBlockVersion :: BlockVersion
    , vIHandlersIn   :: [HandlerSpec]
    , vIHandlersOut  :: [HandlerSpec]
    } deriving (Show, Generic)

data VerAck = VerAck
    deriving (Show, Generic)
