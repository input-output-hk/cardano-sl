{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

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
       , checkCertSign
       , checkCertTTL
       , verifyCommitment
       , verifyCommitmentSignature
       , verifySignedCommitment
       , verifyOpening
       , checkShare
       , checkShares
       , checkOpeningMatchesCommitment
       -- * GtPayload
       , verifyGtPayload

       -- * Modern
       , getThreshold
       ) where

import           Control.Lens                   ((^.))
import           Control.Monad.Fail             (MonadFail)
import           Data.Containers                (ContainerKey, SetContainer (notMember))
import qualified Data.HashMap.Strict            as HM
import qualified Data.HashSet                   as HS (fromList, size)
import           Data.Ix                        (inRange)
import           Data.List.NonEmpty             (NonEmpty (..))
import           Serokell.Util                  (VerificationRes, verifyGeneric)
import           Serokell.Util.Verify           (isVerSuccess)
import           Universum

import           Pos.Binary.Class               (Bi)
import           Pos.Binary.Crypto              ()
import           Pos.Constants                  (k, vssMaxTTL)
import           Pos.Crypto                     (EncShare, PublicKey, Secret, SecretKey,
                                                 SecureRandom (..), Share, Threshold,
                                                 VssPublicKey, checkSig, encShareId,
                                                 genSharedSecret, getDhSecret,
                                                 secretToDhSecret, sign, toPublic,
                                                 verifyEncShare, verifySecretProof,
                                                 verifyShare)
import           Pos.Ssc.Class.Types            (Ssc (..))
import           Pos.Ssc.GodTossing.Types.Base  (Commitment (..), CommitmentsMap,
                                                 InnerSharesMap, Opening (..),
                                                 SignedCommitment, VssCertificate (..),
                                                 VssCertificatesMap)
import           Pos.Ssc.GodTossing.Types.Types (GtGlobalState (..), GtPayload (..))
import qualified Pos.Ssc.GodTossing.VssCertData as VCD
import           Pos.Types.Address              (AddressHash, addressHash)
import           Pos.Types.Types                (EpochIndex (..), LocalSlotIndex,
                                                 MainBlockHeader, SharedSeed (..),
                                                 SlotId (..), headerSlot)
import           Pos.Util                       (AsBinary, asBinary, fromBinaryM)
----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------
-- | Convert Secret to SharedSeed.
secretToSharedSeed :: Secret -> SharedSeed
secretToSharedSeed = SharedSeed . getDhSecret . secretToDhSecret

-- | Generate securely random SharedSeed.
genCommitmentAndOpening
    :: (MonadFail m, MonadIO m)
    => Threshold -> NonEmpty (AsBinary VssPublicKey) -> m (Commitment, Opening)
genCommitmentAndOpening n pks = do
    pks' <- traverse fromBinaryM pks
    liftIO . runSecureRandom . fmap convertRes . genSharedSecret n $ pks'
  where
    convertRes (extra, secret, proof, shares) =
        ( Commitment
          { commExtra = asBinary extra
          , commProof = asBinary proof
          , commShares = HM.fromList $ zip (toList pks) $ map asBinary shares
          }
        , Opening $ asBinary secret)

-- | Make signed commitment from commitment and epoch index using secret key.
mkSignedCommitment
    :: Bi Commitment
    => SecretKey -> EpochIndex -> Commitment -> SignedCommitment
mkSignedCommitment sk i c = (toPublic sk, c, sign sk (i, c))

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

hasCommitment :: AddressHash PublicKey -> GtGlobalState -> Bool
hasCommitment addr = HM.member addr . _gsCommitments

hasOpening :: AddressHash PublicKey -> GtGlobalState -> Bool
hasOpening addr = HM.member addr . _gsOpenings

hasShares :: AddressHash PublicKey -> GtGlobalState -> Bool
hasShares addr = HM.member addr . _gsShares

hasVssCertificate :: AddressHash PublicKey -> GtGlobalState -> Bool
hasVssCertificate addr = VCD.member addr . _gsVssCertificates

----------------------------------------------------------------------------
-- Verifications for GodTossing.Types.Base
----------------------------------------------------------------------------

-- CHECK: @verifyCommitment
-- | Verify that Commitment is correct.
--
-- #verifyEncShare
verifyCommitment :: Commitment -> Bool
verifyCommitment Commitment {..} = fromMaybe False $ do
    extra <- fromBinaryM commExtra
    commMap <- traverse tupleFromBinaryM (HM.toList commShares)
    let encShares = map encShareId . toList <$> commMap
    return $ all (verifyCommitmentDo extra) commMap &&
        (length encShares) == (HS.size $ HS.fromList encShares)
  where
    verifyCommitmentDo extra = uncurry (verifyEncShare extra)
    tupleFromBinaryM
        :: (AsBinary VssPublicKey, AsBinary EncShare)
        -> Maybe (VssPublicKey, EncShare)
    tupleFromBinaryM =
        uncurry (liftA2 (,)) . bimap fromBinaryM fromBinaryM

-- CHECK: @verifyCommitmentPK
-- | Verify public key contained in SignedCommitment against given address
--
-- #checkPubKeyAddress
verifyCommitmentPK :: AddressHash PublicKey -> SignedCommitment -> Bool
verifyCommitmentPK addr (pk, _, _) = addressHash pk == addr

-- CHECK: @verifyCommitmentSignature
-- | Verify signature in SignedCommitment using epoch index.
--
-- #checkSig
verifyCommitmentSignature :: Bi Commitment => EpochIndex -> SignedCommitment -> Bool
verifyCommitmentSignature epoch (pk, comm, commSig) =
    checkSig pk (epoch, comm) commSig

-- CHECK: @verifySignedCommitment
-- | Verify SignedCommitment using public key and epoch index.
--
-- #verifyCommitmentPK
-- #verifyCommitmentSignature
-- #verifyCommitment
verifySignedCommitment
    :: Bi Commitment
    => AddressHash PublicKey
    -> EpochIndex
    -> SignedCommitment
    -> VerificationRes
verifySignedCommitment addr epoch sc@(_, comm, _) =
    verifyGeneric
        [ ( verifyCommitmentPK addr sc
          , "commitment's signing key does not match given address")
        , ( verifyCommitmentSignature epoch sc
          , "commitment has bad signature (e. g. for wrong epoch)")
        , ( verifyCommitment comm
          , "commitment itself is bad (e. g. bad shares")
        ]

-- CHECK: @verifyOpening
-- | Verify that Secret provided with Opening corresponds to given commitment.
--
-- #verifySecretProof
verifyOpening :: Commitment -> Opening -> Bool
verifyOpening Commitment {..} (Opening secret) = fromMaybe False $
    verifySecretProof
      <$> fromBinaryM commExtra
      <*> fromBinaryM secret
      <*> fromBinaryM commProof

-- CHECK: @checkCertSign
-- | Check that the VSS certificate is signed properly
-- #checkPubKeyAddress
-- #checkSig
checkCertSign :: (AddressHash PublicKey, VssCertificate) -> Bool
checkCertSign (addr, VssCertificate {..}) =
    addressHash vcSigningKey == addr &&
    checkSig vcSigningKey (vcVssKey, expiryEpoch) vcSignature

-- CHECK: @checkCertTTL
-- | Check that the VSS certificate has valid TTL:
-- more than 0 and less than vssMaxTTL
checkCertTTL :: EpochIndex -> VssCertificate -> Bool
checkCertTTL curEpochIndex VssCertificate{..} =
    expiryEpoch >= curEpochIndex &&
    getEpochIndex expiryEpoch < vssMaxTTL + getEpochIndex curEpochIndex

-- CHECK: @checkShare
-- | Check that the decrypted share matches the encrypted share in the
-- commitment
--
-- #verifyShare
checkShare :: (SetContainer set, ContainerKey set ~ AddressHash PublicKey)
           => CommitmentsMap
           -> set --set of opening's addresses
           -> VssCertificatesMap
           -> (AddressHash PublicKey, AddressHash PublicKey, AsBinary Share)
           -> Bool
checkShare globalCommitments globalOpeningsPK globalCertificates (addrTo, addrFrom, share) =
    fromMaybe False $ case tuple of
      Just (eS, pk, s) -> verifyShare
                            <$> fromBinaryM eS
                            <*> fromBinaryM pk
                            <*> fromBinaryM s
      _ -> return False
  where
    tuple = do
        -- addrFrom sent its decrypted share to addrTo on commitment phase.
        -- addrTo must decrypt share from addrFrom on shares phase,
        -- if addrFrom didn't send its opening

        -- CHECK: Check that addrFrom really didn't send its opening
        guard $ notMember addrFrom globalOpeningsPK
        -- CHECK: Check that addrFrom really sent its commitment
        (_, comm, _) <- HM.lookup addrFrom globalCommitments
        -- Get pkTo's vss certificate
        vssKey <- vcVssKey <$> HM.lookup addrTo globalCertificates
        -- Get encrypted share, which sent pkFrom to pkTo on commitment phase
        encShare <- HM.lookup vssKey (commShares comm)
        return (encShare, vssKey, share)

-- CHECK: @checkShares
-- Apply checkShare to all shares in map.
--
-- #checkShare
checkShares :: (SetContainer set, ContainerKey set ~ AddressHash PublicKey)
            => CommitmentsMap
            -> set --set of opening's PK. TODO Should we add phantom type for more typesafety?
            -> VssCertificatesMap
            -> AddressHash PublicKey
            -> InnerSharesMap
            -> Bool
checkShares globalCommitments globalOpeningsPK globalCertificates addrTo shares =
    let listShares :: [(AddressHash PublicKey, AddressHash PublicKey, AsBinary Share)]
        listShares = map convert $ HM.toList shares
        convert (addrFrom, share) = (addrTo, addrFrom, share)
    in all
           (checkShare globalCommitments globalOpeningsPK globalCertificates)
           listShares

-- CHECK: @checkOpeningMatchesCommitment
-- | Check that the secret revealed in the opening matches the secret proof
-- in the commitment
checkOpeningMatchesCommitment
    :: CommitmentsMap -> (AddressHash PublicKey, Opening) -> Bool
checkOpeningMatchesCommitment globalCommitments (addr, opening) =
      case HM.lookup addr globalCommitments of
        Nothing           -> False
        Just (_, comm, _) -> verifyOpening comm opening

----------------------------------------------------------------------------
-- GtPayload Part
----------------------------------------------------------------------------

-- CHECK: @verifyGtPayLoad
-- Verify payload using header containing this payload.
--
-- For each DS datum we check:
--
--   1. Whether it's stored in the correct block (e.g. commitments have to be in
--      first k blocks, etc.)
--
--   2. Whether the message itself is correct (e.g. commitment signature is
--      valid, etc.)
--
-- We also do some general sanity checks.
verifyGtPayload
    :: (SscPayload ssc ~ GtPayload, Bi Commitment)
    => MainBlockHeader ssc -> SscPayload ssc -> VerificationRes
verifyGtPayload header payload =
    verifyGeneric $ otherChecks ++
        case payload of
            CommitmentsPayload comms certs ->   isComm
                                              : commChecks comms
                                              : certsChecks certs
            OpeningsPayload        _ certs -> isOpen : certsChecks certs
            SharesPayload          _ certs -> isShare : certsChecks certs
            CertificatesPayload      certs -> isOther : certsChecks certs
  where
    slotId  = header ^. headerSlot
    epochIndex = siEpoch slotId
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
    -- CHECK: For commitments specifically, we also
    --
    --   * check there are only commitments in the block
    --   * use verifySignedCommitment, which checks commitments themselves,
    --     e.g. checks their signatures (which includes checking that the
    --     commitment has been generated for this particular epoch)
    --
    -- #verifySignedCommitment
    commChecks commitments =
        (let checkSignedComm =
                 isVerSuccess .
                 uncurry (flip verifySignedCommitment epochId)
          in all checkSignedComm (HM.toList commitments),
            "verifySignedCommitment has failed for some commitments")
        -- [CSL-206]: check that share IDs are different.

    -- CHECK: Vss certificates checker
    --
    --   * VSS certificates are signed properly
    --
    -- #checkCert
    certsChecks certs = [
        (all checkCertSign (HM.toList certs),
            "some VSS certificates aren't signed properly")
      , (all (checkCertTTL epochIndex) (toList certs),
            "some VSS certificates have invalid TTL")
      ]

    -- CHECK: For all blocks (no matter the type), we check that
    --
    --   * slot ID is in range
    otherChecks =
        [ (inRange (0, 6 * k - 1) (siSlot slotId),
            "slot id is outside of [0, 6k)")]

----------------------------------------------------------------------------
-- Modern
----------------------------------------------------------------------------

-- | Figure out the threshold (i.e. how many secret shares would be required
-- to recover each node's secret) using number of participants.
getThreshold :: Integral a => a -> Threshold
getThreshold len = fromIntegral $ len `div` 2 + len `mod` 2
