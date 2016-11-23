-- | Messages used for communication in GodTossing SSC.

module Pos.Ssc.GodTossing.Types.Message
       ( MsgTag (..)
       , isGoodSlotForTag
       , isGoodSlotIdForTag
       , InvMsg (..)
       , ReqMsg (..)
       , DataMsg (..)
       , dataMsgPublicKey
       , dataMsgTag
       ) where

import           Control.TimeWarp.Rpc          (Message (..))
import           Data.Binary                   (Binary)
import           Data.List.NonEmpty            (NonEmpty)
import qualified Data.Text.Buildable
import           Universum

import           Pos.Crypto                    (PublicKey, Share)
import           Pos.Ssc.GodTossing.Functions  (isCommitmentId, isCommitmentIdx,
                                                isOpeningId, isOpeningIdx, isSharesId,
                                                isSharesIdx)
import           Pos.Ssc.GodTossing.Types.Base (Opening, SignedCommitment, VssCertificate)
import           Pos.Types                     (LocalSlotIndex, SlotId)

-- | Tag associated with message.
data MsgTag
    = CommitmentMsg
    | OpeningMsg
    | SharesMsg
    | VssCertificateMsg
    deriving (Show, Generic)

instance Binary MsgTag

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

-- | Inventory message. Can be used to announce the fact that you have
-- some data.
data InvMsg = InvMsg
    { imType :: !MsgTag
    , imKeys :: !(NonEmpty PublicKey)
    } deriving (Generic)

instance Binary InvMsg

instance Message InvMsg where
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
                   !SignedCommitment
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

-- | MsgTag appropriate for given DataMsg.
dataMsgTag :: DataMsg -> MsgTag
dataMsgTag (DMCommitment _ _)     = CommitmentMsg
dataMsgTag (DMOpening _ _)        = OpeningMsg
dataMsgTag (DMShares _ _)         = SharesMsg
dataMsgTag (DMVssCertificate _ _) = VssCertificateMsg

-- | PublicKey stored in DataMsg.
dataMsgPublicKey :: DataMsg -> PublicKey
dataMsgPublicKey (DMCommitment pk _)     = pk
dataMsgPublicKey (DMOpening pk _)        = pk
dataMsgPublicKey (DMShares pk _)         = pk
dataMsgPublicKey (DMVssCertificate pk _) = pk
