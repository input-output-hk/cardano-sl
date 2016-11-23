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
       , checkOpening
       -- * GtPayload
       , verifyGtPayload
       , filterGtPayload
       ) where

import           Control.Lens                   ((^.))
import qualified Data.HashMap.Strict            as HM
import           Data.Ix                        (inRange)
import           Data.List.NonEmpty             (NonEmpty (..))
import           Pos.Constants                  (k)
import           Pos.Crypto                     (PublicKey, Secret, SecretKey,
                                                 SecureRandom (..), Share, Signed (..),
                                                 Threshold, VssPublicKey, genSharedSecret,
                                                 getDhSecret, secretToDhSecret, sign,
                                                 verify, verifyEncShare,
                                                 verifySecretProof, verifyShare)
import           Pos.Ssc.Class.Types            (Ssc (..))
import           Pos.Ssc.GodTossing.Types.Base  (Commitment (..), CommitmentsMap,
                                                 Opening (..), OpeningsMap,
                                                 SignedCommitment, VssCertificate,
                                                 VssCertificatesMap)
import           Pos.Ssc.GodTossing.Types.Types (GtPayload (..))
import           Pos.Types.Types                (EpochIndex, LocalSlotIndex,
                                                 MainBlockHeader, SharedSeed (..),
                                                 SlotId (..), headerSlot)
import           Serokell.Util                  (VerificationRes, verifyGeneric)
import           Serokell.Util.Verify           (isVerSuccess)
import           Universum

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------
-- | Convert Secret to SharedSeed.
secretToSharedSeed :: Secret -> SharedSeed
secretToSharedSeed = SharedSeed . getDhSecret . secretToDhSecret

-- | Generate securely random SharedSeed.
genCommitmentAndOpening
    :: MonadIO m
    => Threshold -> NonEmpty VssPublicKey -> m (Commitment, Opening)
genCommitmentAndOpening n pks =
    liftIO . runSecureRandom . fmap convertRes . genSharedSecret n $ pks
  where
    convertRes (extra, secret, proof, shares) =
        ( Commitment
          { commExtra = extra
          , commProof = proof
          , commShares = HM.fromList $ zip (toList pks) shares
          }
        , Opening secret)

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

hasCommitment :: PublicKey -> GtPayload -> Bool
hasCommitment pk = HM.member pk . _mdCommitments

hasOpening :: PublicKey -> GtPayload -> Bool
hasOpening pk = HM.member pk . _mdOpenings

hasShares :: PublicKey -> GtPayload -> Bool
hasShares pk = HM.member pk . _mdShares

hasVssCertificate :: PublicKey -> GtPayload -> Bool
hasVssCertificate pk = HM.member pk . _mdVssCertificates

----------------------------------------------------------------------------
-- Verifications for GodTossing.Types.Base
----------------------------------------------------------------------------
-- | Verify that Commitment is correct.
verifyCommitment :: Commitment -> Bool
verifyCommitment Commitment {..} = all verifyCommitmentDo $ HM.toList commShares
  where
    verifyCommitmentDo = uncurry (verifyEncShare commExtra)

-- | Verify signature in SignedCommitment using public key and epoch index.
verifyCommitmentSignature :: PublicKey -> EpochIndex -> SignedCommitment -> Bool
verifyCommitmentSignature pk epoch (comm, commSig) =
    verify pk (epoch, comm) commSig

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
verifyOpening Commitment {..} (Opening secret) =
    verifySecretProof commExtra secret commProof

-- | Check that the VSS certificate is signed properly
checkCert
    :: (PublicKey, VssCertificate)
    -> Bool
checkCert (pk, cert) = verify pk (signedValue cert) (signedSig cert)

-- | Check that the decrypted share matches the encrypted share in the
-- commitment
checkShare
    :: CommitmentsMap
    -> OpeningsMap
    -> VssCertificatesMap
    -> (PublicKey, PublicKey, Share)
    -> Bool
checkShare globalCommitments globalOpenings globalCertificates (pkTo, pkFrom, share) =
    fromMaybe False $ do
        guard $ HM.member pkTo globalCommitments
        guard $ isNothing $ HM.lookup pkFrom globalOpenings
        (comm, _) <- HM.lookup pkFrom globalCommitments
        vssKey <- signedValue <$> HM.lookup pkTo globalCertificates
        encShare <- HM.lookup vssKey (commShares comm)
        return $ verifyShare encShare vssKey share

-- Apply checkShare to all shares in map.
checkShares
    :: CommitmentsMap
    -> OpeningsMap
    -> VssCertificatesMap
    -> PublicKey
    -> HashMap PublicKey Share
    -> Bool
checkShares globalCommitments globalOpenings globalCertificates pkTo shares =
    let listShares :: [(PublicKey, PublicKey, Share)]
        listShares = map convert $ HM.toList shares
        convert (pkFrom, share) = (pkTo, pkFrom, share)
    in all
           (checkShare globalCommitments globalOpenings globalCertificates)
           listShares

-- | Check that the secret revealed in the opening matches the secret proof
-- in the commitment
checkOpening
    :: CommitmentsMap -> (PublicKey, Opening) -> Bool
checkOpening globalCommitments (pk, opening) =
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
verifyGtPayload header GtPayload {..} =
    verifyGeneric allChecks
  where
    slotId       = header ^. headerSlot
    epochId      = siEpoch slotId
    commitments  = _mdCommitments
    openings     = _mdOpenings
    shares       = _mdShares
    certificates = _mdVssCertificates
    isComm       = isCommitmentId slotId
    isOpen       = isOpeningId slotId
    isShare      = isSharesId slotId

    -- We *forbid* blocks from having commitments/openings/shares in blocks
    -- with wrong slotId (instead of merely discarding such commitments/etc)
    -- because it's the miner's responsibility not to include them into the
    -- block if they're late.
    --
    -- For commitments specifically, we also
    --   * check there are only commitments in the block
    --   * use verifySignedCommitment, which checks commitments themselves, e. g.
    --     checks their signatures (which includes checking that the
    --     commitment has been generated for this particular epoch)
    -- TODO: we might also check that all share IDs are different, because
    -- then we would be able to simplify 'calculateSeed' a bit – however,
    -- it's somewhat complicated because we have encrypted shares, shares in
    -- commitments, etc.
    commChecks =
        [ (null openings,
                "there are openings in a commitment block")
        , (null shares,
                "there are shares in a commitment block")
        , (let checkSignedComm = isVerSuccess .
                    uncurry (flip verifySignedCommitment epochId)
            in all checkSignedComm (HM.toList commitments),
                "verifySignedCommitment has failed for some commitments")
        ]

    -- For openings, we check that
    --   * there are only openings in the block
    openChecks =
        [ (null commitments,
                "there are commitments in an openings block")
        , (null shares,
                "there are shares in an openings block")
        ]

    -- For shares, we check that
    --   * there are only shares in the block
    shareChecks =
        [ (null commitments,
                "there are commitments in a shares block")
        , (null openings,
                "there are openings in a shares block")
        ]

    -- For all other blocks, we check that
    --   * there are no commitments, openings or shares
    otherBlockChecks =
        [ (null commitments,
                "there are commitments in an ordinary block")
        , (null openings,
                "there are openings in an ordinary block")
        , (null shares,
                "there are shares in an ordinary block")
        ]

    -- For all blocks (no matter the type), we check that
    --   * slot ID is in range
    --   * VSS certificates are signed properly
    otherChecks =
        [ (inRange (0, 6 * k - 1) (siSlot slotId),
                "slot id is outside of [0, 6k)")
        , (all checkCert (HM.toList certificates),
                "some VSS certificates aren't signed properly")
        ]

    allChecks = concat $ concat
        [ [ commChecks       | isComm ]
        , [ openChecks       | isOpen ]
        , [ shareChecks      | isShare ]
        , [ otherBlockChecks | all not [isComm, isOpen, isShare] ]
        , [ otherChecks ]
        ]


-- | Remove messages irrelevant to given slot id from payload.
filterGtPayload :: SlotId -> GtPayload -> GtPayload
filterGtPayload slotId GtPayload {..} =
    GtPayload
    { _mdCommitments = filteredCommitments
    , _mdOpenings = filteredOpenings
    , _mdShares = filteredShares
    , ..
    }
  where
    filteredCommitments = filterDo isCommitmentId _mdCommitments
    filteredOpenings = filterDo isOpeningId _mdOpenings
    filteredShares = filterDo isSharesId _mdShares
    filterDo
        :: Monoid container
        => (SlotId -> Bool) -> container -> container
    filterDo checker container
        | checker slotId = container
        | otherwise = mempty
