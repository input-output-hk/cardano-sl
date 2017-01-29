{-# LANGUAGE TypeFamilies #-}

-- | Core functions from GodTossing SSC.

module Pos.Ssc.GodTossing.Core.Core
       (
         -- * Helpers
         genCommitmentAndOpening
       , isCommitmentId
       , isCommitmentIdx
       , isOpeningId
       , isOpeningIdx
       , isSharesId
       , isSharesIdx
       , mkSignedCommitment
       , secretToSharedSeed

       -- * CommitmentsMap
       , insertSignedCommitment
       , diffCommMap
       , intersectCommMap
       , intersectCommMapWith

       -- * Verification and Checks
       , checkCertTTL
       , verifyCommitment
       , verifyCommitmentSignature
       , verifySignedCommitment
       , verifyOpening
       , checkShare
       , checkShares
       , checkCommShares

       -- * Payload and proof
       , _gpCertificates
       , mkGtProof
       ) where

import           Data.Containers                (ContainerKey, SetContainer (notMember))
import qualified Data.HashMap.Strict            as HM
import qualified Data.HashSet                   as HS
import           Data.Ix                        (inRange)
import qualified Data.Text.Buildable
import           Data.Text.Lazy.Builder         (Builder)
import           Formatting                     (Format, bprint, (%))
import           Serokell.Util                  (VerificationRes, listJson, verifyGeneric)
import           Universum

import           Pos.Binary.Class               (Bi)
import           Pos.Binary.Crypto              ()
import           Pos.Binary.Ssc.GodTossing.Core ()
import           Pos.Constants                  (blkSecurityParam, vssMaxTTL, vssMinTTL)
import           Pos.Crypto                     (EncShare, Secret, SecretKey,
                                                 SecureRandom (..), Share, Threshold,
                                                 VssPublicKey, checkSig, encShareId,
                                                 genSharedSecret, getDhSecret, hash,
                                                 secretToDhSecret, sign, toPublic,
                                                 verifyEncShare, verifySecretProof,
                                                 verifyShare)
import           Pos.Ssc.GodTossing.Core.Types  (Commitment (..),
                                                 CommitmentsMap (getCommitmentsMap),
                                                 GtPayload (..), GtProof (..),
                                                 InnerSharesMap, Opening (..),
                                                 SignedCommitment,
                                                 VssCertificate (vcExpiryEpoch, vcVssKey),
                                                 VssCertificatesMap,
                                                 mkCommitmentsMapUnsafe)
import           Pos.Types.Address              (addressHash)
import           Pos.Types.Core                 (EpochIndex (..), LocalSlotIndex,
                                                 SlotId (..), StakeholderId)
import           Pos.Types.Types                (SharedSeed (..))
import           Pos.Util                       (AsBinary, asBinary, fromBinaryM, getKeys)

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

----------------------------------------------------------------------------
-- CommitmentsMap
----------------------------------------------------------------------------

-- | Safely insert 'SignedCommitment' into 'CommitmentsMap'.
insertSignedCommitment :: SignedCommitment -> CommitmentsMap -> CommitmentsMap
insertSignedCommitment signedComm (getCommitmentsMap -> m) =
    mkCommitmentsMapUnsafe $
    HM.insert (addressHash $ signedComm ^. _1) signedComm m

-- | Compute difference of two 'CommitmentsMap's.
diffCommMap :: CommitmentsMap -> CommitmentsMap -> CommitmentsMap
diffCommMap (getCommitmentsMap -> a) (getCommitmentsMap -> b) =
    mkCommitmentsMapUnsafe $ a `HM.difference` b

-- | Compute intersection of two 'CommitmentsMap's.
intersectCommMap :: CommitmentsMap -> CommitmentsMap -> CommitmentsMap
intersectCommMap = intersectCommMapWith getCommitmentsMap

-- | Generalized version of 'intersectCommMap' which makes it possible
-- to intersect with different maps.
intersectCommMapWith :: (map -> HashMap StakeholderId x)
                     -> CommitmentsMap
                     -> map
                     -> CommitmentsMap
intersectCommMapWith f (getCommitmentsMap -> a) (f -> b) =
    mkCommitmentsMapUnsafe $ a `HM.intersection` b

----------------------------------------------------------------------------
-- Verifications
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
-- #verifyCommitmentSignature
-- #verifyCommitment
verifySignedCommitment
    :: Bi Commitment
    => EpochIndex
    -> SignedCommitment
    -> VerificationRes
verifySignedCommitment epoch sc@(_, comm, _) =
    verifyGeneric
        [ ( verifyCommitmentSignature epoch sc
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

-- CHECK: @checkCertTTL
-- | Check that the VSS certificate has valid TTL: i. e. it is in
-- '[vssMinTTL, vssMaxTTL]'.
checkCertTTL :: EpochIndex -> VssCertificate -> Bool
checkCertTTL curEpochIndex vc =
    expiryEpoch + 1 >= vssMinTTL + curEpochIndex &&
    expiryEpoch < vssMaxTTL + curEpochIndex
  where
    expiryEpoch = vcExpiryEpoch vc

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
        (_, comm, _) <- HM.lookup addrFrom $ getCommitmentsMap globalCommitments
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

checkCommShares :: [AsBinary VssPublicKey] -> SignedCommitment -> Bool
checkCommShares vssPublicKeys c =
    HS.fromList vssPublicKeys == (getKeys . commShares $ c ^. _2)

----------------------------------------------------------------------------
-- Payload and proof
----------------------------------------------------------------------------

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

-- | Construct 'GtProof' from 'GtPayload'.
mkGtProof :: GtPayload -> GtProof
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
