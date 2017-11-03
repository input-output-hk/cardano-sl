-- | Types related to Toss.

module Pos.Ssc.Toss.Types
       ( SscTag (..)
       , isGoodSlotForTag
       , isGoodSlotIdForTag

       , TossModifier (..)
       , tmCommitments
       , tmOpenings
       , tmShares
       , tmCertificates
       ) where

import           Control.Lens            (makeLenses)
import qualified Data.Text.Buildable     as Buildable
import           Universum

import           Pos.Core                (HasConfiguration, LocalSlotIndex, SlotId,
                                          VssCertificatesMap)
import           Pos.Ssc.Core            (CommitmentsMap, OpeningsMap, SharesMap,
                                          isCommitmentId, isCommitmentIdx, isOpeningId,
                                          isOpeningIdx, isSharesId, isSharesIdx)

-- | Tag corresponding to SSC data.
data SscTag
    = CommitmentMsg
    | OpeningMsg
    | SharesMsg
    | VssCertificateMsg
    deriving (Show, Eq, Generic)

instance Buildable SscTag where
    build CommitmentMsg     = "commitment"
    build OpeningMsg        = "opening"
    build SharesMsg         = "shares"
    build VssCertificateMsg = "VSS certificate"

isGoodSlotForTag :: HasConfiguration => SscTag -> LocalSlotIndex -> Bool
isGoodSlotForTag CommitmentMsg     = isCommitmentIdx
isGoodSlotForTag OpeningMsg        = isOpeningIdx
isGoodSlotForTag SharesMsg         = isSharesIdx
isGoodSlotForTag VssCertificateMsg = const True

isGoodSlotIdForTag :: HasConfiguration => SscTag -> SlotId -> Bool
isGoodSlotIdForTag CommitmentMsg     = isCommitmentId
isGoodSlotIdForTag OpeningMsg        = isOpeningId
isGoodSlotIdForTag SharesMsg         = isSharesId
isGoodSlotIdForTag VssCertificateMsg = const True

data TossModifier = TossModifier
    { _tmCommitments  :: !CommitmentsMap
    , _tmOpenings     :: !OpeningsMap
    , _tmShares       :: !SharesMap
    , _tmCertificates :: !VssCertificatesMap
    } deriving (Generic, Show, Eq)

makeLenses ''TossModifier

instance Monoid TossModifier where
    mempty = TossModifier mempty mempty mempty mempty
    mappend (TossModifier leftComms leftOpens leftShares leftCerts)
            (TossModifier rightComms rightOpens rightShares rightCerts) =
        TossModifier
        { _tmCommitments = rightComms <> leftComms
        , _tmOpenings = rightOpens <> leftOpens
        , _tmShares = rightShares <> leftShares
        , _tmCertificates = rightCerts <> leftCerts
        }

instance Semigroup TossModifier
