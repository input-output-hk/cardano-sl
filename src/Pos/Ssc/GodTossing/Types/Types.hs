{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Some types related to GodTossing necessary for Ssc instance.

module Pos.Ssc.GodTossing.Types.Types
       (
         -- * Instance types
         GtPayload (..)
       , GtProof (..)
       , GtGlobalState (..)
       , GtContext (..)
       , GtParams (..)
       , GtSecretStorage (..)
       , TossVerFailure (..)
       , TossVerErrorTag (..)

       -- * Lenses
       -- ** GtPayload
       , gsCommitments
       , gsOpenings
       , gsShares
       , gsVssCertificates
       , mkGtProof
       , createGtContext
       , _gpCertificates
       ) where

import           Control.Concurrent.STM         (newTVarIO)
import qualified Control.Concurrent.STM         as STM
import           Control.Lens                   (makeLenses)
import           Data.Default                   (Default, def)
import qualified Data.HashMap.Strict            as HM
import qualified Data.Text                      as T
import qualified Data.Text.Buildable
import           Data.Text.Lazy.Builder         (Builder, fromText)
import           Formatting                     (Format, bprint, build, sformat, stext,
                                                 (%))
import           Serokell.Util                  (listJson)
import           Universum

import           Pos.Binary.Class               (Bi)
import           Pos.Crypto                     (Hash, VssKeyPair, hash)
import           Pos.Ssc.GodTossing.Core        (CommitmentsMap (getCommitmentsMap),
                                                 Opening, OpeningsMap, SharesMap,
                                                 SignedCommitment, VssCertificate,
                                                 VssCertificatesMap)
import qualified Pos.Ssc.GodTossing.VssCertData as VCD
import           Pos.Types                      (EpochIndex, SlotId, StakeholderId)

----------------------------------------------------------------------------
-- SscGlobalState
----------------------------------------------------------------------------

-- | Global state of GodTossing, contains relevant SSC data from blocks.
data GtGlobalState = GtGlobalState
    { -- | Commitments are added during the first phase of epoch.
      _gsCommitments     :: !CommitmentsMap
      -- | Openings are added during the second phase of epoch.
    , _gsOpenings        :: !OpeningsMap
      -- | Decrypted shares to be used in the third phase.
    , _gsShares          :: !SharesMap
      -- | Vss certificates are added at any time if they are valid and
      -- received from stakeholders.
    , _gsVssCertificates :: !VCD.VssCertData
    } deriving (Eq, Show, Generic)

makeLenses ''GtGlobalState

instance Default GtGlobalState where
    def =
        GtGlobalState
        {
          _gsCommitments = mempty
        , _gsOpenings = mempty
        , _gsShares = mempty
        , _gsVssCertificates = VCD.empty
        }

instance Buildable GtGlobalState where
    build GtGlobalState {..} =
        formatMPC $ mconcat
            [ formatCommitments
            , formatOpenings
            , formatShares
            , formatCertificates
            ]
      where
        formatMPC :: Text -> Builder
        formatMPC msg
            | T.null msg = "  no MPC data"
            | otherwise = fromText msg
        formatIfNotNull formatter l
            | null l = mempty
            | otherwise = sformat formatter l
        formatCommitments =
            formatIfNotNull
                ("  commitments from: "%listJson%"\n")
                (HM.keys $ getCommitmentsMap _gsCommitments)
        formatOpenings =
            formatIfNotNull
                ("  openings from: "%listJson%"\n")
                (HM.keys _gsOpenings)
        formatShares =
            formatIfNotNull
                ("  shares from: "%listJson%"\n")
                (HM.keys _gsShares)
        formatCertificates =
            formatIfNotNull
                ("  certificates from: "%listJson%"\n")
                (VCD.keys _gsVssCertificates)

----------------------------------------------------------------------------
-- SscPayload
----------------------------------------------------------------------------

-- | Block payload
data GtPayload
    = CommitmentsPayload  !CommitmentsMap !VssCertificatesMap
    | OpeningsPayload     !OpeningsMap    !VssCertificatesMap
    | SharesPayload       !SharesMap      !VssCertificatesMap
    | CertificatesPayload !VssCertificatesMap
    deriving (Eq, Show, Generic)

_gpCertificates :: GtPayload -> VssCertificatesMap
_gpCertificates (CommitmentsPayload _ certs) = certs
_gpCertificates (OpeningsPayload _ certs)    = certs
_gpCertificates (SharesPayload _ certs)      = certs
_gpCertificates (CertificatesPayload certs)  = certs

isEmptyGtPayload :: GtPayload -> Bool
isEmptyGtPayload (CommitmentsPayload comms certs) = null comms && null certs
isEmptyGtPayload (OpeningsPayload opens certs)    = null opens && null certs
isEmptyGtPayload (SharesPayload shares certs)     = null shares && null certs
isEmptyGtPayload (CertificatesPayload certs)      = null certs

instance Buildable GtPayload where
    build gp
        | isEmptyGtPayload gp = "  no GodTossing payload"
        | otherwise =
            case gp of
                CommitmentsPayload comms certs ->
                    formatTwo formatCommitments comms certs
                OpeningsPayload openings certs ->
                    formatTwo formatOpenings openings certs
                SharesPayload shares certs ->
                    formatTwo formatShares shares certs
                CertificatesPayload certs -> formatCertificates certs
      where
        formatIfNotNull
            :: Container c
            => Format Builder (c -> Builder) -> c -> Builder
        formatIfNotNull formatter l
            | null l = mempty
            | otherwise = bprint formatter l
        formatCommitments comms =
            formatIfNotNull
                ("  commitments from: " %listJson % "\n")
                (HM.keys $ getCommitmentsMap comms)
        formatOpenings openings =
            formatIfNotNull
                ("  openings from: " %listJson % "\n")
                (HM.keys openings)
        formatShares shares =
            formatIfNotNull
                ("  shares from: " %listJson % "\n")
                (HM.keys shares)
        formatCertificates certs =
            formatIfNotNull
                ("  certificates from: " %listJson % "\n")
                (HM.keys certs)
        formatTwo formatter hm certs =
            mconcat [formatter hm, formatCertificates certs]

----------------------------------------------------------------------------
-- SscProof
----------------------------------------------------------------------------

-- | Proof of MpcData.
-- We can use ADS for commitments, openings, shares as well,
-- if we find it necessary.
data GtProof
    = CommitmentsProof !(Hash CommitmentsMap) !(Hash VssCertificatesMap)
    | OpeningsProof !(Hash OpeningsMap) !(Hash VssCertificatesMap)
    | SharesProof !(Hash SharesMap) !(Hash VssCertificatesMap)
    | CertificatesProof !(Hash VssCertificatesMap)
    deriving (Show, Eq, Generic)

-- | Smart constructor for 'GtProof' from 'GtPayload'.
mkGtProof
    :: (Bi VssCertificate, Bi Opening, Bi CommitmentsMap)
    => GtPayload -> GtProof
mkGtProof payload =
    case payload of
        CommitmentsPayload comms certs ->
            proof CommitmentsProof comms certs
        OpeningsPayload openings certs ->
            proof OpeningsProof openings certs
        SharesPayload shares certs     ->
            proof SharesProof shares certs
        CertificatesPayload certs      ->
            CertificatesProof $ hash certs
      where
        proof constr hm cert =
            constr (hash hm) (hash cert)

data GtParams = GtParams
    { gtpSscEnabled :: !Bool              -- ^ Whether node should participate in SSC
                                          -- in case SSC requires participation.
    , gtpVssKeyPair :: !VssKeyPair        -- ^ Key pair used for secret sharing
    }

data GtContext = GtContext
    {
      -- | Vss key pair used for MPC.
      gtcVssKeyPair     :: !VssKeyPair
    , -- | Flag which determines whether we want to participate in SSC.
      gtcParticipateSsc :: !(STM.TVar Bool)
    }

createGtContext :: MonadIO m => GtParams -> m GtContext
createGtContext GtParams {..} =
    GtContext gtpVssKeyPair <$> liftIO (newTVarIO gtpSscEnabled)

----------------------------------------------------------------------------
-- Secret storage
----------------------------------------------------------------------------

data GtSecretStorage = GtSecretStorage
    { -- | Our commitment.
      gssCommitment :: !(SignedCommitment)
    , -- | Corresponding opening
      gssOpening    :: !Opening
    , -- | Epoch for which this secret were generated
      gssEpoch      :: !EpochIndex
    } deriving (Show, Eq)

----------------------------------------------------------------------------
-- Verification failure
----------------------------------------------------------------------------
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
    build (TossInternallError msg) =
        bprint ("internal error: "%stext) msg
