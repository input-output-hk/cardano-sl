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

import           Pos.Binary.Class               (Bi)
import           Pos.Binary.Crypto              ()
import           Pos.Constants                  (k)
import           Pos.Crypto                     (EncShare, Share, VssPublicKey, Secret,
                                                 SecretKey, SecureRandom (..), Threshold,
                                                 checkSig, genSharedSecret, getDhSecret,
                                                 secretToDhSecret, sign, toPublic,
                                                 verifyEncShare, verifySecretProof,
                                                 verifyShare)
import           Pos.Ssc.Class.Types            (Ssc (..))
import           Pos.Ssc.GodTossing.Types.Base  (Commitment (..), CommitmentsMap,
                                                 InnerSharesMap, Opening (..),
                                                 SignedCommitment, VssCertificate (..),
                                                 VssCertificatesMap)
import           Pos.Ssc.GodTossing.Types.Types (GtGlobalState (..), GtPayload (..))
import           Pos.Types.Types                (Address (..), EpochIndex, LocalSlotIndex,
                                                 MainBlockHeader, SharedSeed (..),
                                                 SlotId (..), checkPubKeyAddress,
                                                 headerSlot)
import           Pos.Util                       (AsBinary, fromBinaryM, diffDoubleMap,
                                                 asBinary)

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

hasCommitment :: Address -> GtGlobalState -> Bool
hasCommitment addr = HM.member addr . _gsCommitments

hasOpening :: Address -> GtGlobalState -> Bool
hasOpening addr = HM.member addr . _gsOpenings

hasShares :: Address -> GtGlobalState -> Bool
hasShares addr = HM.member addr . _gsShares

hasVssCertificate :: Address -> GtGlobalState -> Bool
hasVssCertificate addr = HM.member addr . _gsVssCertificates

----------------------------------------------------------------------------
-- Verifications for GodTossing.Types.Base
----------------------------------------------------------------------------
-- | Verify that Commitment is correct.
verifyCommitment :: Commitment -> Bool
verifyCommitment Commitment {..} = fromMaybe False $ do
    extra <- fromBinaryM commExtra
    all (verifyCommitmentDo extra) <$> traverse tupleFromBinaryM (HM.toList commShares)
  where
    verifyCommitmentDo extra = uncurry (verifyEncShare extra)
    tupleFromBinaryM
        :: (AsBinary VssPublicKey, AsBinary EncShare)
        -> Maybe (VssPublicKey, EncShare)
    tupleFromBinaryM =
        uncurry (liftA2 (,)) . bimap fromBinaryM fromBinaryM

-- | Verify public key contained in SignedCommitment against given address
verifyCommitmentPK :: Address -> SignedCommitment -> Bool
verifyCommitmentPK addr (pk, _, _) = checkPubKeyAddress pk addr

-- | Verify signature in SignedCommitment using epoch index.
verifyCommitmentSignature :: Bi Commitment => EpochIndex -> SignedCommitment -> Bool
verifyCommitmentSignature epoch (pk, comm, commSig) =
    checkSig pk (epoch, comm) commSig

-- | Verify SignedCommitment using public key and epoch index.
verifySignedCommitment
    :: Bi Commitment
    => Address -> EpochIndex -> SignedCommitment -> VerificationRes
verifySignedCommitment addr epoch sc@(_, comm, _) =
    verifyGeneric
        [ ( verifyCommitmentPK addr sc
          , "commitment's signing key does not match given address")
        , ( verifyCommitmentSignature epoch sc
          , "commitment has bad signature (e. g. for wrong epoch)")
        , ( verifyCommitment comm
          , "commitment itself is bad (e. g. bad shares")
        ]

-- | Verify that Secret provided with Opening corresponds to given commitment.
verifyOpening :: Commitment -> Opening -> Bool
verifyOpening Commitment {..} (Opening secret) = fromMaybe False $
    verifySecretProof
      <$> fromBinaryM commExtra
      <*> fromBinaryM secret
      <*> fromBinaryM commProof

-- | Check that the VSS certificate is signed properly
checkCert :: (Address, VssCertificate) -> Bool
checkCert (addr, VssCertificate {..}) =
    checkPubKeyAddress vcSigningKey addr &&
    checkSig vcSigningKey vcVssKey vcSignature

-- | Check that the decrypted share matches the encrypted share in the
-- commitment
checkShare :: (SetContainer set, ContainerKey set ~ Address)
           => CommitmentsMap
           -> set --set of opening's addresses
           -> VssCertificatesMap
           -> (Address, Address, AsBinary Share)
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

        -- Check that addrFrom really didn't send its opening
        guard $ notMember addrFrom globalOpeningsPK
        -- Check that addrFrom really sent its commitment
        (_, comm, _) <- HM.lookup addrFrom globalCommitments
        -- Get pkTo's vss certificate
        vssKey <- vcVssKey <$> HM.lookup addrTo globalCertificates
        -- Get encrypted share, which sent pkFrom to pkTo on commitment phase
        encShare <- HM.lookup vssKey (commShares comm)
        return (encShare, vssKey, share)

-- Apply checkShare to all shares in map.
checkShares :: (SetContainer set, ContainerKey set ~ Address)
            => CommitmentsMap
            -> set --set of opening's PK. TODO Should we add phantom type for more typesafety?
            -> VssCertificatesMap
            -> Address
            -> InnerSharesMap
            -> Bool
checkShares globalCommitments globalOpeningsPK globalCertificates addrTo shares =
    let listShares :: [(Address, Address, AsBinary Share)]
        listShares = map convert $ HM.toList shares
        convert (addrFrom, share) = (addrTo, addrFrom, share)
    in all
           (checkShare globalCommitments globalOpeningsPK globalCertificates)
           listShares

-- | Check that the secret revealed in the opening matches the secret proof
-- in the commitment
checkOpeningMatchesCommitment
    :: CommitmentsMap -> (Address, Opening) -> Bool
checkOpeningMatchesCommitment globalCommitments (addr, opening) =
      case HM.lookup addr globalCommitments of
        Nothing           -> False
        Just (_, comm, _) -> verifyOpening comm opening

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
    :: (SscPayload ssc ~ GtPayload, Bi Commitment)
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
            let filteredOpenings =
                    foldl' (flip ($)) opens $
                    [
                    -- Select only new openings
                      (`HM.difference` _gsOpenings)
                    -- Select commitments which sent opening
                    , (`HM.intersection` _gsCommitments)
                    -- Select opening which corresponds its commitment
                    , HM.filterWithKey
                          (curry $ checkOpeningMatchesCommitment _gsCommitments)
                    ]
            in
                OpeningsPayload filteredOpenings (filterCerts certs)
        SharesPayload shares certs ->
            let filteredShares =
                    foldl' (flip ($)) shares $
                    [
                    -- Select only new shares
                      (`diffDoubleMap` _gsShares)
                    -- Select shares from nodes which sent certificates
                    , (`HM.intersection` _gsVssCertificates)
                    -- Select shares to nodes which sent commitments
                    , map (`HM.intersection` _gsCommitments)
                    -- Ensure that share sent from pkFrom to pkTo is valid
                    , HM.mapWithKey filterShares
                    ]
            in
                SharesPayload filteredShares (filterCerts certs)
        CertificatesPayload certs -> CertificatesPayload $ filterCerts certs
  where
    filterCerts = flip HM.difference _gsVssCertificates
    filterShares pkTo shares = HM.filterWithKey
        (\pkFrom share -> checkShare
                              _gsCommitments
                              _gsOpenings
                              _gsVssCertificates
                              (pkTo, pkFrom, share)) shares
