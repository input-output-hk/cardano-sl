-- | Messages used for communication in GodTossing SSC.

module Pos.Ssc.GodTossing.Types.Message
       ( MsgTag (..)
       , isGoodSlotForTag
       , isGoodSlotIdForTag
       , InvMsg (..)
       , ReqMsg (..)
       , DataMsg (..)
       , dataMsgNodeId
       , dataMsgTag
       ) where

import           Control.TimeWarp.Rpc          (Message (..), messageName')
import           Data.List.NonEmpty            (NonEmpty)
import qualified Data.Text.Buildable
import           Universum

import           Pos.Ssc.GodTossing.Functions  (isCommitmentId, isCommitmentIdx,
                                                isOpeningId, isOpeningIdx, isSharesId,
                                                isSharesIdx)
import           Pos.Ssc.GodTossing.Types.Base (InnerSharesMap, Opening, SignedCommitment,
                                                VssCertificate)
import           Pos.Types                     (LocalSlotIndex, NodeId, SlotId)

-- | Tag associated with message.
data MsgTag
    = CommitmentMsg
    | OpeningMsg
    | SharesMsg
    | VssCertificateMsg
    deriving (Show, Eq, Generic)

instance Buildable MsgTag where
    build CommitmentMsg     = "commitment message"
    build OpeningMsg        = "opening message"
    build SharesMsg         = "shares message"
    build VssCertificateMsg = "VSS certificate message"

isGoodSlotForTag :: MsgTag -> LocalSlotIndex -> Bool
isGoodSlotForTag CommitmentMsg     = isCommitmentIdx
isGoodSlotForTag OpeningMsg        = isOpeningIdx
isGoodSlotForTag SharesMsg         = isSharesIdx
isGoodSlotForTag VssCertificateMsg = const True

isGoodSlotIdForTag :: MsgTag -> SlotId -> Bool
isGoodSlotIdForTag CommitmentMsg     = isCommitmentId
isGoodSlotIdForTag OpeningMsg        = isOpeningId
isGoodSlotIdForTag SharesMsg         = isSharesId
isGoodSlotIdForTag VssCertificateMsg = const True

-- [CSL-203]: this model assumes that shares and commitments are
-- always sent as a single message, it allows to identify such
-- messages using only Address.

-- | Inventory message. Can be used to announce the fact that you have
-- some data.
data InvMsg = InvMsg
    { imType  :: !MsgTag
    , imNodes :: !(NonEmpty NodeId)
    } deriving (Show, Eq, Generic)

instance Message InvMsg where
    messageName _ = "GT Inventory"
    formatMessage = messageName'

-- | Request message. Can be used to request data (ideally data which
-- was previously announced by inventory message).
data ReqMsg = ReqMsg
    { rmType :: !MsgTag
    , rmNode :: !NodeId
    } deriving (Show, Eq, Generic)

instance Message ReqMsg where
    messageName _ = "GT Request"
    formatMessage = messageName'

-- | Data message. Can be used to send actual data.
data DataMsg
    = DMCommitment !NodeId
                   !SignedCommitment
    | DMOpening !NodeId
                !Opening
    | DMShares !NodeId
               InnerSharesMap
    | DMVssCertificate !NodeId
                       !VssCertificate
    deriving (Show, Eq, Generic)

instance Message DataMsg where
    messageName _ = "GT Data"
    formatMessage = messageName'

-- | MsgTag appropriate for given DataMsg.
dataMsgTag :: DataMsg -> MsgTag
dataMsgTag (DMCommitment _ _)     = CommitmentMsg
dataMsgTag (DMOpening _ _)        = OpeningMsg
dataMsgTag (DMShares _ _)         = SharesMsg
dataMsgTag (DMVssCertificate _ _) = VssCertificateMsg

-- | Node ID stored in DataMsg.
dataMsgNodeId :: DataMsg -> NodeId
dataMsgNodeId (DMCommitment nid _)     = nid
dataMsgNodeId (DMOpening nid _)        = nid
dataMsgNodeId (DMShares nid _)         = nid
dataMsgNodeId (DMVssCertificate nid _) = nid
