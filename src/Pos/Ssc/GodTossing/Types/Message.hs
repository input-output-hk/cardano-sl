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

import           Control.TimeWarp.Rpc          (Message (..), messageName')
import           Data.Binary                   (Binary)
import           Data.List.NonEmpty            (NonEmpty)
import qualified Data.Text.Buildable
import           Universum

import           Pos.Ssc.GodTossing.Functions  (isCommitmentId, isCommitmentIdx,
                                                isOpeningId, isOpeningIdx, isSharesId,
                                                isSharesIdx)
import           Pos.Ssc.GodTossing.Types.Base (InnerSharesMap, Opening,
                                                SignedCommitment, VssCertificate)
import           Pos.Types                     (Address, LocalSlotIndex, SlotId)

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

-- [CSL-203]: this model assumes that shares and commitments are
-- always sent as a single message, it allows to identify such
-- messages using only Address.

-- | Inventory message. Can be used to announce the fact that you have
-- some data.
data InvMsg = InvMsg
    { imType :: !MsgTag
    , imKeys :: !(NonEmpty Address)
    } deriving (Generic)

instance Binary InvMsg

instance Message InvMsg where
    messageName _ = "GT Inventory"
    formatMessage = messageName'

-- | Request message. Can be used to request data (ideally data which
-- was previously announced by inventory message).
data ReqMsg = ReqMsg
    { rmType :: !MsgTag
    , rmKey  :: !Address
    } deriving (Generic)

instance Binary ReqMsg

instance Message ReqMsg where
    messageName _ = "GT Request"
    formatMessage = messageName'

-- | Data message. Can be used to send actual data.
data DataMsg
    = DMCommitment !Address
                   !SignedCommitment
    | DMOpening !Address
                !Opening
    | DMShares !Address
               InnerSharesMap
    | DMVssCertificate !Address
                       !VssCertificate
    deriving (Generic)

instance Binary DataMsg

instance Message DataMsg where
    messageName _ = "GT Data"
    formatMessage = messageName'

-- | MsgTag appropriate for given DataMsg.
dataMsgTag :: DataMsg -> MsgTag
dataMsgTag (DMCommitment _ _)     = CommitmentMsg
dataMsgTag (DMOpening _ _)        = OpeningMsg
dataMsgTag (DMShares _ _)         = SharesMsg
dataMsgTag (DMVssCertificate _ _) = VssCertificateMsg

-- | Address stored in DataMsg.
dataMsgPublicKey :: DataMsg -> Address
dataMsgPublicKey (DMCommitment addr _)     = addr
dataMsgPublicKey (DMOpening addr _)        = addr
dataMsgPublicKey (DMShares addr _)         = addr
dataMsgPublicKey (DMVssCertificate addr _) = addr
