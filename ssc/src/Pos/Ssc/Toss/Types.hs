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

import           Control.Lens (makeLenses)
import qualified Formatting.Buildable as Buildable
import           Universum

import           Pos.Binary.Class (Cons (..), Field (..), deriveSimpleBi,
                     deriveSimpleBiCxt)
import           Pos.Core (HasProtocolConstants, LocalSlotIndex, SlotId,
                     VssCertificatesMap)
import           Pos.Core.Ssc (CommitmentsMap, OpeningsMap, SharesMap)
import           Pos.Ssc.Base (isCommitmentId, isCommitmentIdx, isOpeningId,
                     isOpeningIdx, isSharesId, isSharesIdx)
import           Pos.Util.Util (cborError)

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

deriveSimpleBi ''SscTag [
    Cons 'CommitmentMsg [],
    Cons 'OpeningMsg [],
    Cons 'SharesMsg [],
    Cons 'VssCertificateMsg []]

isGoodSlotForTag :: HasProtocolConstants => SscTag -> LocalSlotIndex -> Bool
isGoodSlotForTag CommitmentMsg     = isCommitmentIdx
isGoodSlotForTag OpeningMsg        = isOpeningIdx
isGoodSlotForTag SharesMsg         = isSharesIdx
isGoodSlotForTag VssCertificateMsg = const True

isGoodSlotIdForTag :: HasProtocolConstants => SscTag -> SlotId -> Bool
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

instance Semigroup TossModifier where
   (TossModifier leftComms leftOpens leftShares leftCerts)
     <> (TossModifier rightComms rightOpens rightShares rightCerts) =
        TossModifier
        { _tmCommitments = rightComms <> leftComms
        , _tmOpenings = rightOpens <> leftOpens
        , _tmShares = rightShares <> leftShares
        , _tmCertificates = rightCerts <> leftCerts
        }

instance Monoid TossModifier where
    mempty = TossModifier mempty mempty mempty mempty
    mappend = (<>)

deriveSimpleBiCxt [t|()|] ''TossModifier [
    Cons 'TossModifier [
        Field [| _tmCommitments  :: CommitmentsMap     |],
        Field [| _tmOpenings     :: OpeningsMap        |],
        Field [| _tmShares       :: SharesMap          |],
        Field [| _tmCertificates :: VssCertificatesMap |]
    ]]
