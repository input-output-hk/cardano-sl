{-# LANGUAGE TemplateHaskell #-}

-- | Types related to Toss.

module Pos.Ssc.GodTossing.Toss.Types
       ( GtTag (..)
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

import           Pos.Core                (LocalSlotIndex, SlotId)
import           Pos.Ssc.GodTossing.Core (CommitmentsMap, OpeningsMap, SharesMap,
                                          VssCertificatesMap, isCommitmentId,
                                          isCommitmentIdx, isOpeningId, isOpeningIdx,
                                          isSharesId, isSharesIdx)

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

isGoodSlotForTag :: GtTag -> LocalSlotIndex -> Bool
isGoodSlotForTag CommitmentMsg     = isCommitmentIdx
isGoodSlotForTag OpeningMsg        = isOpeningIdx
isGoodSlotForTag SharesMsg         = isSharesIdx
isGoodSlotForTag VssCertificateMsg = const True

isGoodSlotIdForTag :: GtTag -> SlotId -> Bool
isGoodSlotIdForTag CommitmentMsg     = isCommitmentId
isGoodSlotIdForTag OpeningMsg        = isOpeningId
isGoodSlotIdForTag SharesMsg         = isSharesId
isGoodSlotIdForTag VssCertificateMsg = const True

data TossModifier = TossModifier
    { _tmCommitments  :: !CommitmentsMap
    , _tmOpenings     :: !OpeningsMap
    , _tmShares       :: !SharesMap
    , _tmCertificates :: !VssCertificatesMap
    } deriving (Show, Eq)

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
