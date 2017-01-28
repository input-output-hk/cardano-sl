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
       , checkCertSign
       , checkCertTTL
       , verifyCommitment
       , verifyCommitmentSignature
       , verifySignedCommitment
       , verifyOpening
       , checkShare
       , checkShares
       , checkOpeningMatchesCommitment
       , checkCommShares
       -- * GtPayload
       , verifyGtPayload
       -- * Verification helper
       , verifyEntriesGuard

       -- * VSS
       , vssThreshold
       , computeParticipants
       , getStableCertsPure
       ) where

import           Control.Lens                   (at)
import           Control.Monad.Except           (MonadError (throwError), runExcept)
import           Data.Containers                (ContainerKey, SetContainer (notMember))
import qualified Data.HashMap.Strict            as HM
import qualified Data.HashSet                   as HS
import           Data.Ix                        (inRange)
import qualified Data.List.NonEmpty             as NE
import           Formatting                     (build, sformat, (%))
import           Serokell.Util                  (VerificationRes, verifyGeneric)
import           Serokell.Util.Verify           (isVerSuccess)
import           Universum

import           Pos.Binary.Class               (Bi)
import           Pos.Binary.Crypto              ()
import           Pos.Constants                  (blkSecurityParam, vssMaxTTL, vssMinTTL)
import           Pos.Crypto                     (EncShare, Secret, SecretKey,
                                                 SecureRandom (..), Share, Threshold,
                                                 VssPublicKey, checkSig, encShareId,
                                                 genSharedSecret, getDhSecret,
                                                 secretToDhSecret, sign, toPublic,
                                                 verifyEncShare, verifySecretProof,
                                                 verifyShare)
import           Pos.Lrc.Types                  (Richmen)
import           Pos.Ssc.Class.Types            (Ssc (..))
import           Pos.Ssc.GodTossing.Genesis     (genesisCertificates)
import           Pos.Ssc.GodTossing.Types.Base  (Commitment (..), CommitmentsMap,
                                                 InnerSharesMap, Opening (..),
                                                 SignedCommitment, VssCertificate (..),
                                                 VssCertificatesMap)
import           Pos.Ssc.GodTossing.Types.Types (GtGlobalState (..), GtPayload (..),
                                                 TossVerErrorTag (..),
                                                 TossVerFailure (..), gsCommitments)
import qualified Pos.Ssc.GodTossing.VssCertData as VCD
import           Pos.Types.Address              (addressHash)
import           Pos.Types.Core                 (EpochIndex (..), LocalSlotIndex,
                                                 SlotId (..), StakeholderId)
import           Pos.Types.Slotting             (crucialSlot)
import           Pos.Types.Types                (MainBlockHeader, SharedSeed (..),
                                                 headerSlot)
import           Pos.Util                       (AsBinary, asBinary, fromBinaryM, getKeys)

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
genCommitmentAndOpening n pks
    | n <= 0 = fail "genCommitmentAndOpening: threshold must be positive"
    | otherwise = do
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
isCommitmentIdx = inRange (0, 2 * blkSecurityParam - 1)

isOpeningIdx :: LocalSlotIndex -> Bool
isOpeningIdx = inRange (4 * blkSecurityParam, 6 * blkSecurityParam - 1)

isSharesIdx :: LocalSlotIndex -> Bool
isSharesIdx = inRange (8 * blkSecurityParam, 10 * blkSecurityParam - 1)

isCommitmentId :: SlotId -> Bool
isCommitmentId = isCommitmentIdx . siSlot

isOpeningId :: SlotId -> Bool
isOpeningId = isOpeningIdx . siSlot

isSharesId :: SlotId -> Bool
isSharesId = isSharesIdx . siSlot

-- Not the best solution :(
hasCommitment
    :: Bi Commitment
    => EpochIndex -> StakeholderId -> GtGlobalState -> Bool
hasCommitment epoch id gs =
    case gs ^. gsCommitments . at id of
        Nothing              -> False
        Just (pk, comm, sig) -> checkSig pk (epoch, comm) sig

hasOpening :: StakeholderId -> GtGlobalState -> Bool
hasOpening addr = HM.member addr . _gsOpenings

hasShares :: StakeholderId -> GtGlobalState -> Bool
hasShares addr = HM.member addr . _gsShares

hasVssCertificate :: StakeholderId -> GtGlobalState -> Bool
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
    comms <- traverse tupleFromBinaryM (HM.toList commShares)
    let encShares = map (encShareId . snd) comms
    return $ all (verifyCommitmentDo extra) comms &&
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
verifyCommitmentPK :: StakeholderId -> SignedCommitment -> Bool
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
    => StakeholderId
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
checkCertSign :: (StakeholderId, VssCertificate) -> Bool
checkCertSign (addr, VssCertificate {..}) =
    addressHash vcSigningKey == addr &&
    checkSig vcSigningKey (vcVssKey, vcExpiryEpoch) vcSignature

-- CHECK: @checkCertTTL
-- | Check that the VSS certificate has valid TTL:
-- more than 0 and less than vssMaxTTL
checkCertTTL :: EpochIndex -> VssCertificate -> Bool
checkCertTTL curEpochIndex VssCertificate{..} =
    vcExpiryEpoch + 1 >= vssMinTTL + curEpochIndex &&
    getEpochIndex vcExpiryEpoch < vssMaxTTL + getEpochIndex curEpochIndex

-- CHECK: @checkShare
-- | Check that the decrypted share matches the encrypted share in the
-- commitment
--
-- #verifyShare
checkShare :: (SetContainer set, ContainerKey set ~ StakeholderId)
           => CommitmentsMap
           -> set --set of opening's addresses
           -> VssCertificatesMap
           -> (StakeholderId, StakeholderId, AsBinary Share)
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
        -- Get encrypted share, which was sent from pkFrom to pkTo on commitment phase
        encShare <- HM.lookup vssKey (commShares comm)
        return (encShare, vssKey, share)

-- CHECK: @checkShares
-- Apply checkShare to all shares in map.
--
-- #checkShare
checkShares :: (SetContainer set, ContainerKey set ~ StakeholderId)
            => CommitmentsMap
            -> set --set of opening's PK. TODO Should we add phantom type for more typesafety?
            -> VssCertificatesMap
            -> StakeholderId
            -> InnerSharesMap
            -> Bool
checkShares globalCommitments globalOpeningsPK globalCertificates addrTo shares =
    let listShares :: [(StakeholderId, StakeholderId, AsBinary Share)]
        listShares = map convert $ HM.toList shares
        convert (addrFrom, share) = (addrTo, addrFrom, share)
    in all
           (checkShare globalCommitments globalOpeningsPK globalCertificates)
           listShares

-- CHECK: @checkOpeningMatchesCommitment
-- | Check that the secret revealed in the opening matches the secret proof
-- in the commitment
checkOpeningMatchesCommitment
    :: CommitmentsMap -> (StakeholderId, Opening) -> Bool
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
--      first 2 * blkSecurityParam blocks, etc.)
--
--   2. Whether the message itself is correct (e.g. commitment signature is
--      valid, etc.)
--
-- We also do some general sanity checks.
verifyGtPayload
    :: (SscPayload ssc ~ GtPayload, Bi Commitment)
    => MainBlockHeader ssc -> SscPayload ssc -> Either TossVerFailure ()
verifyGtPayload header payload = case payload of
    CommitmentsPayload comms certs -> do
        isComm
        commChecks comms
        certsChecks certs
    OpeningsPayload        _ certs -> do
        isOpen
        certsChecks certs
    SharesPayload          _ certs -> do
        isShare
        certsChecks certs
    CertificatesPayload      certs -> do
        isOther
        certsChecks certs
  where
    slotId  = header ^. headerSlot
    epochIndex = siEpoch slotId
    epochId = siEpoch slotId
    isComm  = unless (isCommitmentId slotId) $ Left $ NotCommitmentPhase slotId
    isOpen  = unless (isOpeningId slotId) $ Left $ NotOpeningPhase slotId
    isShare = unless (isSharesId slotId) $ Left $ NotSharesPhase slotId
    isOther = unless (all not $
                      map ($ slotId) [isCommitmentId, isOpeningId, isSharesId]) $
                      Left tossIE
    tossIE =
        TossInternallError $ sformat (build%" doesn't belong intermediate phase") slotId

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
    commChecks commitments = runExcept $ do
        let checkSignedComm =
                 isVerSuccess .
                 uncurry (flip verifySignedCommitment epochId)
        verifyEntriesGuard fst identity (TossVerFailure CommitmentInvalid)
                           checkSignedComm
                           (HM.toList commitments)
        -- [CSL-206]: check that share IDs are different.

    -- CHECK: Vss certificates checker
    --
    --   * VSS certificates are signed properly
    --   * VSS certificates have valid TTLs
    --
    -- #checkCert
    certsChecks certs = runExcept $ do
        verifyEntriesGuard identity identity CertificateInvalidSign
                           checkCertSign
                           (HM.toList certs)
        verifyEntriesGuard (second vcExpiryEpoch . join (,)) identity CertificateInvalidTTL
                           (checkCertTTL epochIndex)
                           (toList certs)

checkCommShares :: [AsBinary VssPublicKey] -> SignedCommitment -> Bool
checkCommShares vssPublicKeys c =
    HS.fromList vssPublicKeys == (getKeys . commShares $ c ^. _2)

----------------------------------------------------------------------------
-- Verification helper
----------------------------------------------------------------------------

-- It takes list of entries ([(StakeholderId, v)] or [StakeholderId]),
-- function condition and error tag (fKey and fValue - see below)
-- If condition is true for every entry - function does nothing.
-- Otherwise it gets all entries which don't pass condition
-- and throwError with [StakeholderId] corresponding to these entries.
-- fKey is needed for getting StakeholderId from entry.
-- fValue is needed for getting value which must be tested by condition function.
verifyEntriesGuard
    :: MonadError TossVerFailure m
    => (entry -> key)
    -> (entry -> verificationVal)
    -> (NonEmpty key -> TossVerFailure)
    -> (verificationVal -> Bool)
    -> [entry]
    -> m ()
verifyEntriesGuard fKey fVal exception cond =
    maybeThrowError exception .
    NE.nonEmpty .
    map fKey .
    filter (not . cond . fVal)
  where
    maybeThrowError _ Nothing    = pass
    maybeThrowError er (Just ne) = throwError $ er ne

----------------------------------------------------------------------------
-- Modern
----------------------------------------------------------------------------

-- | Figure out the threshold (i.e. how many secret shares would be required
-- to recover each node's secret) using number of participants.
vssThreshold :: Integral a => a -> Threshold
vssThreshold len = fromIntegral $ len `div` 2 + len `mod` 2

computeParticipants :: Richmen -> VssCertificatesMap -> VssCertificatesMap
computeParticipants (HS.toMap . HS.fromList . NE.toList -> richmen) =
    (`HM.intersection` richmen)

getStableCertsPure :: EpochIndex -> VCD.VssCertData -> VssCertificatesMap
getStableCertsPure epoch certs
    | epoch == 0 = genesisCertificates
    | otherwise =
          VCD.certs $ VCD.setLastKnownSlot (crucialSlot epoch) certs
