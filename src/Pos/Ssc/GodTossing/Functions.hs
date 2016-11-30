{-# LANGUAGE TypeFamilies #-}


module Pos.Ssc.GodTossing.Functions
       (
         -- * Helpers
         genCommitmentAndOpening
       , isCommitmentId
       , isCommitmentIdx
       , isOpeningId
       , isOpeningIdx
       , isSharesId
       , isSharesIdx
       , inLastKSlotsId
       , hasCommitment
       , hasOpening
       , hasShares
       , hasVssCertificate
       , mkSignedCommitment
       , secretToSharedSeed

       -- * Verification and Checks
       , checkCert
       , verifyCommitment
       , verifyCommitmentSignature
       , verifySignedCommitment
       , verifyOpening
       , checkShare
       , checkShares
       , checkOpeningMatchesCommitment
       -- * GtPayload
       , verifyGtPayload
       , filterLocalPayload
       ) where

import           Control.Lens                   ((^.))
import           Control.Monad.Fail             (MonadFail)
import           Data.Containers                (ContainerKey, SetContainer (notMember))
import qualified Data.HashMap.Strict            as HM
import           Data.Ix                        (inRange)
import           Data.List.NonEmpty             (NonEmpty (..))
import           Serokell.Util                  (VerificationRes, verifyGeneric)
import           Serokell.Util.Verify           (isVerSuccess)
import           Universum

import           Pos.Constants                  (k)
import           Pos.Crypto                     (LShare, LVssPublicKey, PublicKey, Secret,
                                                 SecretKey, SecureRandom (..),
                                                 Signed (..), Threshold, genSharedSecret,
                                                 getDhSecret, secretToDhSecret, sign,
                                                 checkSig, verifyEncShare,
                                                 verifySecretProof, verifyShare)
import           Pos.Ssc.Class.Types            (Ssc (..))
import           Pos.Ssc.GodTossing.Types.Base  (Commitment (..), CommitmentsMap,
                                                 Opening (..), SignedCommitment,
                                                 VssCertificate, VssCertificatesMap)
import           Pos.Ssc.GodTossing.Types.Types (GtGlobalState (..), GtPayload (..))
import           Pos.Types.Types                (EpochIndex, LocalSlotIndex,
                                                 MainBlockHeader, SharedSeed (..),
                                                 SlotId (..), headerSlot)
import           Pos.Util                       (deserializeM, diffDoubleMap, serialize)

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------
-- | Convert Secret to SharedSeed.
secretToSharedSeed :: Secret -> SharedSeed
secretToSharedSeed = SharedSeed . getDhSecret . secretToDhSecret

-- | Generate securely random SharedSeed.
genCommitmentAndOpening
    :: (MonadFail m, MonadIO m)
    => Threshold -> NonEmpty LVssPublicKey -> m (Commitment, Opening)
genCommitmentAndOpening n pks = do
    pks' <- traverse deserializeM pks
    liftIO . runSecureRandom . fmap convertRes . genSharedSecret n $ pks'
  where
    convertRes (extra, secret, proof, shares) =
        ( Commitment
          { commExtra = serialize extra
          , commProof = serialize proof
          , commShares = HM.fromList $ zip (toList pks) $ map serialize shares
          }
        , Opening $ serialize secret)

-- | Make signed commitment from commitment and epoch index using secret key.
mkSignedCommitment :: SecretKey -> EpochIndex -> Commitment -> SignedCommitment
mkSignedCommitment sk i c = (c, sign sk (i, c))

----------------------------------------------------------------------------
-- Simple predicates for GodTossing.Types.Base
----------------------------------------------------------------------------
isCommitmentIdx :: LocalSlotIndex -> Bool
isCommitmentIdx = inRange (0, k - 1)

isOpeningIdx :: LocalSlotIndex -> Bool
isOpeningIdx = inRange (2 * k, 3 * k - 1)

isSharesIdx :: LocalSlotIndex -> Bool
isSharesIdx = inRange (4 * k, 5 * k - 1)

isCommitmentId :: SlotId -> Bool
isCommitmentId = isCommitmentIdx . siSlot

isOpeningId :: SlotId -> Bool
isOpeningId = isOpeningIdx . siSlot

isSharesId :: SlotId -> Bool
isSharesId = isSharesIdx . siSlot

inLastKSlotsId :: SlotId -> Bool
inLastKSlotsId SlotId{..} = siSlot >= 5 * k

hasCommitment :: PublicKey -> GtGlobalState -> Bool
hasCommitment pk = HM.member pk . _gsCommitments

hasOpening :: PublicKey -> GtGlobalState -> Bool
hasOpening pk = HM.member pk . _gsOpenings

hasShares :: PublicKey -> GtGlobalState -> Bool
hasShares pk = HM.member pk . _gsShares

hasVssCertificate :: PublicKey -> GtGlobalState -> Bool
hasVssCertificate pk = HM.member pk . _gsVssCertificates

----------------------------------------------------------------------------
-- Verifications for GodTossing.Types.Base
----------------------------------------------------------------------------
-- | Verify that Commitment is correct.
verifyCommitment :: Commitment -> Bool
verifyCommitment Commitment {..} = fromMaybe False $ do
    extra <- deserializeM commExtra
    all (verifyCommitmentDo extra) <$> traverse deserializeM (HM.toList commShares)
  where
    verifyCommitmentDo extra = uncurry (verifyEncShare extra)

-- | Verify signature in SignedCommitment using public key and epoch index.
verifyCommitmentSignature :: PublicKey -> EpochIndex -> SignedCommitment -> Bool
verifyCommitmentSignature pk epoch (comm, commSig) =
    checkSig pk (epoch, comm) commSig

-- | Verify SignedCommitment using public key and epoch index.
verifySignedCommitment :: PublicKey -> EpochIndex -> SignedCommitment -> VerificationRes
verifySignedCommitment pk epoch sc =
    verifyGeneric
        [ ( verifyCommitmentSignature pk epoch sc
          , "commitment has bad signature (e. g. for wrong epoch)")
        , ( verifyCommitment (fst sc)
          , "commitment itself is bad (e. g. bad shares")
        ]

-- | Verify that Secret provided with Opening corresponds to given commitment.
verifyOpening :: Commitment -> Opening -> Bool
verifyOpening Commitment {..} (Opening secret) = fromMaybe False $
    verifySecretProof
      <$> deserializeM commExtra
      <*> deserializeM secret
      <*> deserializeM commProof

-- | Check that the VSS certificate is signed properly
checkCert
    :: (PublicKey, VssCertificate)
    -> Bool
checkCert (pk, cert) = checkSig pk (signedValue cert) (signedSig cert)

-- | Check that the decrypted share matches the encrypted share in the
-- commitment
checkShare :: (SetContainer set, ContainerKey set ~ PublicKey)
           => CommitmentsMap
           -> set --set of opening's PK
           -> VssCertificatesMap
           -> (PublicKey, PublicKey, LShare)
           -> Bool
checkShare globalCommitments globalOpeningsPK globalCertificates (pkTo, pkFrom, share) =
    fromMaybe False $ case tuple of
      Just (eS, pk, s) -> verifyShare
                            <$> deserializeM eS
                            <*> deserializeM pk
                            <*> deserializeM s
      _ -> return False
  where
    tuple = do
        guard $ notMember pkFrom globalOpeningsPK
        (comm, _) <- HM.lookup pkFrom globalCommitments
        vssKey <- signedValue <$> HM.lookup pkTo globalCertificates
        encShare <- HM.lookup vssKey (commShares comm)
        return (encShare, vssKey, share)

-- Apply checkShare to all shares in map.
checkShares :: (SetContainer set, ContainerKey set ~ PublicKey)
            => CommitmentsMap
            -> set --set of opening's PK. TODO Should we add phantom type for more typesafety?
            -> VssCertificatesMap
            -> PublicKey
            -> HashMap PublicKey LShare
            -> Bool
checkShares globalCommitments globalOpeningsPK globalCertificates pkTo shares =
    let listShares :: [(PublicKey, PublicKey, LShare)]
        listShares = map convert $ HM.toList shares
        convert (pkFrom, share) = (pkTo, pkFrom, share)
    in all
           (checkShare globalCommitments globalOpeningsPK globalCertificates)
           listShares

-- | Check that the secret revealed in the opening matches the secret proof
-- in the commitment
checkOpeningMatchesCommitment
    :: CommitmentsMap -> (PublicKey, Opening) -> Bool
checkOpeningMatchesCommitment globalCommitments (pk, opening) =
      case HM.lookup pk globalCommitments of
        Nothing        -> False
        Just (comm, _) -> verifyOpening comm opening

----------------------------------------------------------------------------
-- GtPayload Part
----------------------------------------------------------------------------
{- |

Verify payload using header containing this payload.

For each DS datum we check:

  1. Whether it's stored in the correct block (e.g. commitments have to be in
     first k blocks, etc.)

  2. Whether the message itself is correct (e.g. commitment signature is
     valid, etc.)

We also do some general sanity checks.
-}
verifyGtPayload
    :: (SscPayload ssc ~ GtPayload)
    => MainBlockHeader ssc -> SscPayload ssc -> VerificationRes
verifyGtPayload header payload =
    verifyGeneric $ otherChecks ++
        case payload of
            CommitmentsPayload comms certs -> [ certsChecks certs
                                                 , isComm
                                                 , commChecks comms]
            OpeningsPayload        _ certs -> [certsChecks certs, isOpen]
            SharesPayload          _ certs -> [certsChecks certs, isShare]
            CertificatesPayload      certs -> [certsChecks certs, isOther]
  where
    slotId  = header ^. headerSlot
    epochId = siEpoch slotId
    isComm  = (isCommitmentId slotId, "slotId doesn't belong commitment phase")
    isOpen  = (isOpeningId slotId, "slotId doesn't belong openings phase")
    isShare = (isSharesId slotId, "slotId doesn't belong share phase")
    isOther = (all not $ map fst [isComm, isOpen, isShare],
                  "slotId doesn't belong intermediate phase")

    -- We *forbid* blocks from having commitments/openings/shares in blocks
    -- with wrong slotId (instead of merely discarding such commitments/etc)
    -- because it's the miner's responsibility not to include them into the
    -- block if they're late.
    --
    -- For commitments specifically, we also
    --   * check there are only commitments in the block
    --   * use verifySignedCommitment, which checks commitments themselves,
    --     e.g. checks their signatures (which includes checking that the
    --     commitment has been generated for this particular epoch)
    commChecks commitments =
        (let checkSignedComm =
                 isVerSuccess .
                 uncurry (flip verifySignedCommitment epochId)
          in all checkSignedComm (HM.toList commitments),
            "verifySignedCommitment has failed for some commitments")
        -- [CSL-206]: check that share IDs are different.

    -- Vss certificates checker
    --   * VSS certificates are signed properly
    certsChecks certs =
        (all checkCert (HM.toList certs),
            "some VSS certificates aren't signed properly")

    -- For all blocks (no matter the type), we check that
    --   * slot ID is in range
    otherChecks =
        [ (inRange (0, 6 * k - 1) (siSlot slotId),
            "slot id is outside of [0, 6k)")]

----------------------------------------------------------------------------
-- Filter Local Payload
----------------------------------------------------------------------------
filterLocalPayload :: GtPayload -> GtGlobalState -> GtPayload
filterLocalPayload localPay GtGlobalState {..} =
    case localPay of
        CommitmentsPayload comms certs ->
            CommitmentsPayload
                ((comms `HM.difference` _gsCommitments) `HM.intersection`
                 _gsVssCertificates)
                (filterCerts certs)
        OpeningsPayload opens certs ->
            OpeningsPayload
                ((opens `HM.difference` _gsOpenings) `HM.intersection`
                 _gsCommitments)
                (filterCerts certs)
        SharesPayload shares certs ->
            SharesPayload (shares `diffDoubleMap` _gsShares) (filterCerts certs)
        CertificatesPayload certs -> CertificatesPayload $ filterCerts certs
  where
    filterCerts = flip HM.difference _gsVssCertificates
