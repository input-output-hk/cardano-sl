-- | Possible failures in Toss.

module Pos.Ssc.GodTossing.Toss.Failure
       ( TossVerFailure (..)
       ) where

import qualified Data.Text.Buildable
import           Formatting              (bprint, build, ords, stext, (%))
import           Serokell.Util           (listJson)
import           Universum

import           Pos.Core                (EpochIndex, SlotId, StakeholderId)
import           Pos.Ssc.GodTossing.Core (VssCertificate)

instance Buildable (StakeholderId, VssCertificate) where
    build (a, b) = bprint ("(id: "%build%" , cert: "%build%")") a b

type NEStIds = NonEmpty StakeholderId

data TossVerFailure
    = NotCommitmentPhase !SlotId
    | NotOpeningPhase !SlotId
    | NotSharesPhase !SlotId
    | NotIntermediatePhase !SlotId
    | CurrentSlotUnknown

    | DifferentEpoches !EpochIndex !EpochIndex
    | TossUnknownRichmen !EpochIndex
    | NoRichmen !EpochIndex

    | CommitmentInvalid !NEStIds
    | CommitingNoParticipants !NEStIds
    | CommitmentAlreadySent !NEStIds
    | CommSharesOnWrongParticipants !NEStIds
    | OpeningAlreadySent !NEStIds
    | OpeningWithoutCommitment !NEStIds
    | OpeningNotMatchCommitment !NEStIds
    | SharesNotRichmen !NEStIds
    | InternalShareWithoutCommitment !NEStIds
    | SharesAlreadySent !NEStIds
    | DecrSharesNotMatchCommitment !NEStIds
    | CertificateAlreadySent !NEStIds
    | CertificateNotRichmen  !NEStIds
    | CertificateInvalidSign !(NonEmpty (StakeholderId, VssCertificate))
    | CertificateInvalidTTL !(NonEmpty VssCertificate)

    | TossInternallError !Text
    deriving Eq

instance Buildable TossVerFailure where
    build (NotCommitmentPhase slotId) =
        bprint (build%" doesn't belong commitment phase") slotId
    build (NotOpeningPhase slotId) =
        bprint (build%" doesn't belong openings phase") slotId
    build (NotSharesPhase slotId) =
        bprint (build%" doesn't belong share phase") slotId
    build (NotIntermediatePhase slotId) =
        bprint (build%" doesn't  belong intermidiate phase") slotId
    build CurrentSlotUnknown = "we don't know current slot"

    build (DifferentEpoches e g) =
        bprint ("expected epoch: "%build%", but got: "%build) e g
    build (NoRichmen epoch) =
        bprint ("no richmen for epoch"%build) epoch
    build (TossUnknownRichmen epoch) =
        bprint ("richmen aren't know for "%ords%" epoch") epoch

    build (CommitmentInvalid ids) =
        bprint ("verifySignedCommitment has failed for some commitments: "%listJson) ids
    build (CommitingNoParticipants ids) =
        bprint ("some committing nodes can't be participants: "%listJson) ids
    build (CommitmentAlreadySent ids) =
        bprint ("some nodes have already sent their commitments: "%listJson) ids
    build (CommSharesOnWrongParticipants ids) =
        bprint ("some commShares has been generated on wrong participants: "%listJson) ids
    build (OpeningAlreadySent ids) =
        bprint ("some nodes have already sent their openings: "%listJson) ids
    build (OpeningWithoutCommitment ids) =
        bprint ("some openings don't have corresponding commitments: "%listJson) ids
    build (OpeningNotMatchCommitment ids) =
        bprint ("some openings don't match corresponding commitments: "%listJson) ids
    build (SharesNotRichmen ids) =
        bprint ("some shares are posted by stakeholders that don't have enough stake: "%listJson) ids
    build (InternalShareWithoutCommitment ids) =
        bprint ("some internal share don't have corresponding commitments: "%listJson) ids

    build (CertificateInvalidSign certs) =
        bprint ("some VSS certificates aren't signed properly: "%listJson) certs
    build (CertificateInvalidTTL certs) =
        bprint ("some VSS certificates have invalid TTL: "%listJson) certs
    build (SharesAlreadySent stks) =
        bprint ("some shares have already been sent: "%listJson) stks
    build (DecrSharesNotMatchCommitment stks) =
        bprint ("some decrypted shares don't match encrypted shares \
                \in the corresponding commitment: "%listJson) stks
    build (CertificateAlreadySent stks) =
        bprint ("some VSS certificates have been already sent: "%listJson) stks
    build (CertificateNotRichmen stks) =
        bprint ("some VSS certificates users are not passing stake threshold: "%listJson) stks
    build (TossInternallError msg) =
        bprint ("internal error: "%stext) msg
