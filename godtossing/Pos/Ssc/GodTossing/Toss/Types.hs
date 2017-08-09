-- | Types related to Toss.

module Pos.Ssc.GodTossing.Toss.Types
       ( GtTag (..)
       , isGoodSlotForTag

       , TossModifier (..)
       , tmCommitments
       , tmOpenings
       , tmShares
       , tmCertificates
       ) where

import           Universum

import           Control.Lens            (makeLenses)
import qualified Data.Text.Buildable     as Buildable

import           Pos.Core                (BlockCount, LocalSlotIndex)
import           Pos.Ssc.GodTossing.Core (CommitmentsMap, OpeningsMap, SharesMap,
                                          VssCertificatesMap, isCommitmentIdx,
                                          isOpeningIdx, isSharesIdx)

-- | Tag corresponding to GodTossing data.
data GtTag
    = CommitmentMsg
    | OpeningMsg
    | SharesMsg
    | VssCertificateMsg
    deriving (Show, Eq, Generic)

instance Buildable GtTag where
    build CommitmentMsg     = "commitment"
    build OpeningMsg        = "opening"
    build SharesMsg         = "shares"
    build VssCertificateMsg = "VSS certificate"

isGoodSlotForTag :: BlockCount -> GtTag -> LocalSlotIndex -> Bool
isGoodSlotForTag k CommitmentMsg     = isCommitmentIdx k
isGoodSlotForTag k OpeningMsg        = isOpeningIdx k
isGoodSlotForTag k SharesMsg         = isSharesIdx k
isGoodSlotForTag _ VssCertificateMsg = const True

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
