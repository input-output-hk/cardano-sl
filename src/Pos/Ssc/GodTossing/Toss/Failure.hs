-- | Possible failures in Toss.

module Pos.Ssc.GodTossing.Toss.Failure
       ( TossVerErrorTag (..)
       , TossVerFailure (..)
       ) where

import qualified Data.Text.Buildable
import           Formatting              (bprint, build, ords, stext, (%))
import           Serokell.Util           (listJson)
import           Universum

import           Pos.Ssc.GodTossing.Core (VssCertificate)
import           Pos.Types               (EpochIndex, SlotId, StakeholderId)

data TossVerErrorTag
    = CommitmentInvalid
    | CommitingNoParticipants
    | CommitmentAlreadySent
    | CommSharesOnWrongParticipants
    | OpeningAlreadySent
    | OpeningWithoutCommitment
    | OpeningNotMatchCommitment
    | SharesNotRichmen
    | InternalShareWithoutCommitment
    | SharesAlreadySent
    | DecrSharesNotMatchCommitment
    | CertificateAlreadySent
    | CertificateNotRichmen

instance Buildable (StakeholderId, VssCertificate) where
    build (a, b) = bprint ("(id: "%build%" , cert: "%build%")") a b

instance Buildable (VssCertificate, EpochIndex) where
    build (a, b) = bprint ("(cert: "%build%" , epoch: "%build%")") a b

instance Buildable TossVerErrorTag where
    build CommitmentInvalid =
        bprint "verifySignedCommitment has failed for some commitments"
    build CommitingNoParticipants =
        bprint "some committing nodes can't be participants"
    build CommitmentAlreadySent =
        bprint "some nodes have already sent their commitments"
    build CommSharesOnWrongParticipants =
        bprint "some commShares has been generated on wrong participants"
    build OpeningAlreadySent =
        bprint "some nodes have already sent their openings"
    build OpeningWithoutCommitment =
        bprint "some openings don't have corresponding commitments"
    build OpeningNotMatchCommitment =
        bprint "some openings don't match corresponding commitments"
    build SharesNotRichmen =
        bprint "some shares are posted by stakeholders that don't have enough stake"
    build InternalShareWithoutCommitment =
        bprint "some internal share don't have corresponding commitments"
    build SharesAlreadySent =
        bprint "some shares have already been sent"
    build DecrSharesNotMatchCommitment =
        bprint "some decrypted shares don't match encrypted shares \
                \in the corresponding commitment"
    build CertificateAlreadySent =
        bprint "some VSS certificates have been already sent"
    build CertificateNotRichmen =
        bprint "some VSS certificates' users are not passing stake threshold"

data TossVerFailure
    = TossVerFailure
    { tvfErrorTag     :: !TossVerErrorTag
    , tvfStakeholders :: !(NonEmpty StakeholderId)
    }
    | NotCommitmentPhase !SlotId
    | NotOpeningPhase !SlotId
    | NotSharesPhase !SlotId
    | NotIntermediatePhase !SlotId
    | DifferentEpoches !EpochIndex !EpochIndex
    | CertificateInvalidSign !(NonEmpty (StakeholderId, VssCertificate))
    | CertificateInvalidTTL !(NonEmpty (VssCertificate, EpochIndex))
    | TossUnknownRichmen !EpochIndex
    | NoRichmen !EpochIndex
    | TossInternallError !Text

instance Buildable TossVerFailure where
    build (TossVerFailure tag stks) =
        bprint (build%": "%listJson) tag stks
    build (NotCommitmentPhase slotId) =
        bprint (build%" doesn't belong commitment phase") slotId
    build (NotOpeningPhase slotId) =
        bprint (build%" doesn't belong openings phase") slotId
    build (NotSharesPhase slotId) =
        bprint (build%" doesn't belong share phase") slotId
    build (NotIntermediatePhase slotId) =
        bprint (build%" doesn't  belong intermidiate phase") slotId
    build (DifferentEpoches e g) =
        bprint ("expected epoch: "%build%", but got: "%build) e g
    build (CertificateInvalidSign certs) =
        bprint ("some VSS certificates aren't signed properly: "%listJson) certs
    build (CertificateInvalidTTL certs) =
        bprint ("some VSS certificates have invalid TTL: "%listJson) certs
    build (TossUnknownRichmen epoch) =
        bprint ("richmen aren't know for "%ords%" epoch") epoch
    build (NoRichmen epoch) =
        bprint ("no richmen for epoch"%build) epoch
    build (TossInternallError msg) =
        bprint ("internal error: "%stext) msg
