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
       , deleteSignedCommitment
       , diffCommMap
       , intersectCommMap
       , intersectCommMapWith

       -- * Verification and Checks
       , checkCertTTL
       , verifyCommitmentSignature
       , verifySignedCommitment
       , verifyCommitment
       , verifyOpening

       -- * Payload and proof
       , _gpCertificates
       , mkGtProof
       , stripGtPayload
       , defaultGtPayload
       ) where

import           Universum

import qualified Data.HashMap.Strict            as HM
import qualified Data.HashSet                   as HS
import           Data.Ix                        (inRange)
import qualified Data.List.NonEmpty             as NE
import qualified Data.Text.Buildable
import           Data.Text.Lazy.Builder         (Builder)
import           Formatting                     (Format, bprint, int, (%))
import           Serokell.Data.Memory.Units     (Byte)
import           Serokell.Util                  (VerificationRes, listJson, verifyGeneric)

import           Pos.Binary.Class               (AsBinary, Bi, asBinary, biSize,
                                                 fromBinaryM)
import           Pos.Binary.Crypto              ()
import           Pos.Binary.Ssc.GodTossing.Core ()
import           Pos.Constants                  (blkSecurityParam, vssMaxTTL, vssMinTTL)
import           Pos.Core.Address               (addressHash)
import           Pos.Core.Types                 (EpochIndex (..),
                                                 LocalSlotIndex (getSlotIndex),
                                                 SharedSeed (..), SlotId (..),
                                                 StakeholderId)
import           Pos.Crypto                     (EncShare, Secret, SecretKey,
                                                 SecureRandom (..),
                                                 SignTag (SignCommitment), Threshold,
                                                 VssPublicKey, checkSig, encShareId,
                                                 genSharedSecret, getDhSecret, hash,
                                                 secretToDhSecret, shortHashF, sign,
                                                 toPublic, verifyEncShare,
                                                 verifySecretProof)
import           Pos.Ssc.GodTossing.Core.Types  (Commitment (..),
                                                 CommitmentsMap (getCommitmentsMap),
                                                 GtPayload (..), GtProof (..),
                                                 Opening (..), SignedCommitment,
                                                 VssCertificate (vcExpiryEpoch),
                                                 VssCertificatesMap,
                                                 mkCommitmentsMapUnsafe)
import           Pos.Util.Limits                (stripHashMap)

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
        liftIO . runSecureRandom . fmap (convertRes pks) . genSharedSecret n $ pks'
  where
    convertRes (toList -> ps) (extra, secret, proof, shares) =
        ( Commitment
          { commExtra = asBinary extra
          , commProof = asBinary proof
          , commShares = HM.fromList $ map toPair $ NE.groupWith fst $ zip ps shares
          }
        , Opening $ asBinary secret)
    toPair ne@(x:|_) = (fst x, NE.map (asBinary . snd) ne)

-- | Make signed commitment from commitment and epoch index using secret key.
mkSignedCommitment
    :: Bi Commitment
    => SecretKey -> EpochIndex -> Commitment -> SignedCommitment
mkSignedCommitment sk i c = (toPublic sk, c, sign SignCommitment sk (i, c))

isCommitmentIdx :: LocalSlotIndex -> Bool
isCommitmentIdx = inRange (0, 2 * blkSecurityParam - 1) . getSlotIndex

isOpeningIdx :: LocalSlotIndex -> Bool
isOpeningIdx =
    inRange (4 * blkSecurityParam, 6 * blkSecurityParam - 1) . getSlotIndex

isSharesIdx :: LocalSlotIndex -> Bool
isSharesIdx =
    inRange (8 * blkSecurityParam, 10 * blkSecurityParam - 1) . getSlotIndex

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

-- | Safely delete 'SignedCommitment' from 'CommitmentsMap'.
deleteSignedCommitment :: StakeholderId -> CommitmentsMap -> CommitmentsMap
deleteSignedCommitment id = mkCommitmentsMapUnsafe . HM.delete id . getCommitmentsMap

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
    let encShares = concatMap (map encShareId . toList . snd) comms
    return $ (not . null) commShares &&
        all (verifyCommitmentDo extra) comms &&
        (length encShares) == (HS.size $ HS.fromList encShares)
  where
    verifyCommitmentDo extra (pk, ne) = all (verifyEncShare extra pk) ne
    tupleFromBinaryM
        :: (AsBinary VssPublicKey, NonEmpty (AsBinary EncShare))
        -> Maybe (VssPublicKey, NonEmpty EncShare)
    tupleFromBinaryM =
        uncurry (liftA2 (,)) . bimap fromBinaryM (traverse fromBinaryM)

-- CHECK: @verifyCommitmentSignature
-- | Verify signature in SignedCommitment using epoch index.
--
-- #checkSig
verifyCommitmentSignature :: Bi Commitment => EpochIndex -> SignedCommitment -> Bool
verifyCommitmentSignature epoch (pk, comm, commSig) =
    checkSig SignCommitment pk (epoch, comm) commSig

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
                (map formatVssCert $ HM.toList certs)
        formatVssCert (id, cert) =
            bprint (shortHashF%":"%int) id (vcExpiryEpoch cert)
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

-- | Transforms GtPayload to fit under size limit.
stripGtPayload :: Byte -> GtPayload -> Maybe GtPayload
stripGtPayload lim payload | biSize payload <= lim = Just payload
stripGtPayload lim payload = case payload of
    (CertificatesPayload vssmap) -> CertificatesPayload <$> stripHashMap lim vssmap
    (CommitmentsPayload (getCommitmentsMap -> comms0) certs0) -> do
        let certs = stripHashMap limCerts certs0
        let comms = stripHashMap (lim - biSize certs) comms0
        CommitmentsPayload <$> (mkCommitmentsMapUnsafe <$> comms) <*> certs
    (OpeningsPayload openings0 certs0) -> do
        let certs = stripHashMap limCerts certs0
        let openings = stripHashMap (lim - biSize certs) openings0
        OpeningsPayload <$> openings <*> certs
    (SharesPayload shares0 certs0) -> do
        let certs = stripHashMap limCerts certs0
        let shares = stripHashMap (lim - biSize certs) shares0
        SharesPayload <$> shares <*> certs
  where
    limCerts = lim `div` 3 -- certificates are 1/3 less important than everything else
                           -- this is a random choice in fact

-- | Default godtossing payload depending on local slot index.
defaultGtPayload :: LocalSlotIndex -> GtPayload
defaultGtPayload lsi
    | isCommitmentIdx lsi = CommitmentsPayload mempty mempty
    | isOpeningIdx lsi = OpeningsPayload mempty mempty
    | isSharesIdx lsi = SharesPayload mempty mempty
    | otherwise = CertificatesPayload mempty
