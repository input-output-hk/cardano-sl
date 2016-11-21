-- | Messages used for communication in GodTossing SSC.

module Pos.Ssc.GodTossing.Types.Message
       ( MsgTag (..)
       , InvMsg (..)
       , ReqMsg (..)
       , DataMsg (..)
       ) where

import           Control.TimeWarp.Rpc          (Message (..))
import           Data.Binary                   (Binary)
import           Data.List.NonEmpty            (NonEmpty)
import           Universum

import           Pos.Crypto                    (PublicKey, Share)
import           Pos.Ssc.GodTossing.Types.Base (Commitment, Opening, VssCertificate)

-- | Tag associated with message.
data MsgTag
    = CommitmentMsg
    | OpeningMsg
    | SharesMsg
    | VssCertificateMsg
    deriving (Show, Generic)

instance Binary MsgTag

-- | Inventory message. Can be used to announce the fact that you have
-- some data.
data InvMsg = InvMsg
    { imType :: !MsgTag
    , imKeys :: !(NonEmpty PublicKey)
    } deriving (Generic)

instance Binary InvMsg

instance Message MsgTag where
    messageName _ = "GT Inventory"

-- | Request message. Can be used to request data (ideally data which
-- was previously announced by inventory message).
data ReqMsg = ReqMsg
    { rmType :: !MsgTag
    , rmKey  :: !PublicKey
    } deriving (Generic)

instance Binary ReqMsg

instance Message ReqMsg where
    messageName _ = "GT Request"

-- | Data message. Can be used to send actual data.
data DataMsg
    = DMCommitment !PublicKey
                   !Commitment
    | DMOpening !PublicKey
                !Opening
    | DMShares !PublicKey
               !(HashMap PublicKey Share)
    | DMVssCertificate !PublicKey
                       !VssCertificate
    deriving (Generic)

instance Binary DataMsg

instance Message DataMsg where
    messageName _ = "GT Data"
