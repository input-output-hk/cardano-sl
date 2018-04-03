{-# LANGUAGE TypeFamilies #-}

-- | Core functions from SSC.

module Pos.Ssc.Base
       (
         -- * Helpers
         genCommitmentAndOpening
       , isCommitmentIdExplicit
       , isCommitmentId
       , isCommitmentIdxExplicit
       , isCommitmentIdx
       , isOpeningIdExplicit
       , isOpeningId
       , isOpeningIdxExplicit
       , isOpeningIdx
       , isSharesIdExplicit
       , isSharesId
       , isSharesIdxExplicit
       , isSharesIdx
       , mkSignedCommitment
       , secretToSharedSeed
       , vssThreshold

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
       , stripSscPayload
       , defaultSscPayload
       ) where

import           Universum hiding (id)

import qualified Crypto.Random as Rand
import qualified Data.HashMap.Strict as HM
import           Data.Ix (inRange)
import qualified Data.List.NonEmpty as NE
import           Formatting (build, sformat, (%))
import           Serokell.Data.Memory.Units (Byte)
import           Serokell.Util (VerificationRes, verifyGeneric)

import           Pos.Binary.Class (Bi, asBinary, biSize, fromBinary)
import           Pos.Binary.Core ()
import           Pos.Binary.Crypto ()
import           Pos.Core (EpochIndex (..), LocalSlotIndex, SharedSeed (..), SlotCount, SlotId (..),
                           StakeholderId, addressHash, unsafeMkLocalSlotIndexExplicit)
import           Pos.Core.Configuration (HasProtocolConstants, vssMaxTTL, vssMinTTL,
                                         protocolConstants)
import           Pos.Core.ProtocolConstants (ProtocolConstants (..),
                                             pcSlotSecurityParam)
import           Pos.Core.Ssc (Commitment (..), CommitmentsMap (getCommitmentsMap), Opening (..),
                               SignedCommitment, SscPayload (..), VssCertificate (vcExpiryEpoch),
                               VssCertificatesMap (..), mkCommitmentsMapUnsafe)
import           Pos.Crypto (ProtocolMagic, Secret, SecretKey, SignTag (SignCommitment),
                             Threshold, VssPublicKey, checkSig, genSharedSecret,
                             getDhSecret, secretToDhSecret, sign, toPublic, verifySecret)
import           Pos.Crypto.Configuration (HasProtocolMagic, protocolMagic)
import           Pos.Util.Limits (stripHashMap)

-- | Convert Secret to SharedSeed.
secretToSharedSeed :: Secret -> SharedSeed
secretToSharedSeed = SharedSeed . getDhSecret . secretToDhSecret

-- | Figure out the threshold (i.e. how many secret shares would be required
-- to recover each node's secret) from the total number of shares.
--
-- We use this function to find out what threshold to use when creating
-- shares, when verifying shares, and when recovering the secret.
vssThreshold :: Integral a => a -> Threshold
vssThreshold len = fromIntegral $ len `div` 2 + len `mod` 2

-- | Generate random SharedSeed.
genCommitmentAndOpening
    :: Rand.MonadRandom m
    => Threshold -> NonEmpty VssPublicKey -> m (Commitment, Opening)
genCommitmentAndOpening t pks
    | t <= 1 = error $ sformat
        ("genCommitmentAndOpening: threshold ("%build%") must be > 1") t
    | t >= n - 1 = error $ sformat
        ("genCommitmentAndOpening: threshold ("%build%") must be < n-1"%
         " (n = "%build%")") t n
    | otherwise = convertRes <$> genSharedSecret t pks
  where
    n = fromIntegral (length pks)
    convertRes (secret, proof, shares) =
        ( Commitment
          { commProof = proof
          , commShares = HM.fromList $ map toPair $ NE.groupWith fst shares
          }
        , Opening $ asBinary secret)
    toPair ne@(x:|_) = (asBinary (fst x), NE.map (asBinary . snd) ne)

-- | Make signed commitment from commitment and epoch index using secret key.
mkSignedCommitment
    :: (Bi Commitment)
    => ProtocolMagic -> SecretKey -> EpochIndex -> Commitment -> SignedCommitment
mkSignedCommitment pm sk i c = (toPublic sk, c, sign pm SignCommitment sk (i, c))

toLocalSlotIndex :: ProtocolConstants -> SlotCount -> LocalSlotIndex
toLocalSlotIndex pc = unsafeMkLocalSlotIndexExplicit pc . fromIntegral

isCommitmentIdxExplicit :: ProtocolConstants -> LocalSlotIndex -> Bool
isCommitmentIdxExplicit pc =
    inRange (toLocalSlotIndex pc 0,
             toLocalSlotIndex pc (pcSlotSecurityParam pc - 1))

isCommitmentIdx :: HasProtocolConstants => LocalSlotIndex -> Bool
isCommitmentIdx = isCommitmentIdxExplicit protocolConstants

isOpeningIdxExplicit :: ProtocolConstants -> LocalSlotIndex -> Bool
isOpeningIdxExplicit pc =
    inRange (toLocalSlotIndex pc (2 * pcSlotSecurityParam pc),
             toLocalSlotIndex pc (3 * pcSlotSecurityParam pc - 1))

isOpeningIdx :: HasProtocolConstants => LocalSlotIndex -> Bool
isOpeningIdx = isOpeningIdxExplicit protocolConstants

isSharesIdxExplicit :: ProtocolConstants -> LocalSlotIndex -> Bool
isSharesIdxExplicit pc =
    inRange (toLocalSlotIndex pc (4 * pcSlotSecurityParam pc),
             toLocalSlotIndex pc (5 * pcSlotSecurityParam pc - 1))

isSharesIdx :: HasProtocolConstants => LocalSlotIndex -> Bool
isSharesIdx = isSharesIdxExplicit protocolConstants

isCommitmentIdExplicit :: ProtocolConstants -> SlotId -> Bool
isCommitmentIdExplicit pc = isCommitmentIdxExplicit pc . siSlot

isCommitmentId :: HasProtocolConstants => SlotId -> Bool
isCommitmentId = isCommitmentIdExplicit protocolConstants

isOpeningIdExplicit :: ProtocolConstants -> SlotId -> Bool
isOpeningIdExplicit pc = isOpeningIdxExplicit pc . siSlot

isOpeningId :: HasProtocolConstants => SlotId -> Bool
isOpeningId = isOpeningIdExplicit protocolConstants

isSharesIdExplicit :: ProtocolConstants -> SlotId -> Bool
isSharesIdExplicit pc = isSharesIdxExplicit pc . siSlot

isSharesId :: HasProtocolConstants => SlotId -> Bool
isSharesId = isSharesIdExplicit protocolConstants

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
-- | Verify some /basic/ things about 'Commitment' (like “whether it contains
-- any shares”).
--
-- * We don't check that the commitment is generated for proper set of
--   participants. This is done in 'checkCommitmentShares'.
--
-- * We also don't verify the shares, because that requires 'MonadRandom' and
--   we want to keep this check pure because it's performed in the 'Bi'
--   instance for blocks.
verifyCommitment :: Commitment -> Bool
verifyCommitment Commitment {..} = fromMaybe False $ do
    -- The shares can be deserialized
    mapM_ (rightToMaybe . fromBinary) (HM.keys commShares)
    mapM_ (mapM_ (rightToMaybe . fromBinary)) (HM.elems commShares)
    -- The commitment contains shares
    pure $ not (null commShares)

-- CHECK: @verifyCommitmentSignature
-- | Verify signature in SignedCommitment using epoch index.
--
-- #checkSig
verifyCommitmentSignature
    :: (HasProtocolMagic, Bi Commitment)
    => EpochIndex
    -> SignedCommitment
    -> Bool
verifyCommitmentSignature epoch (pk, comm, commSig) =
    checkSig protocolMagic SignCommitment pk (epoch, comm) commSig

-- CHECK: @verifySignedCommitment
-- | Verify SignedCommitment using public key and epoch index.
--
-- #verifyCommitmentSignature
-- #verifyCommitment
verifySignedCommitment
    :: (HasProtocolMagic, Bi Commitment)
    => EpochIndex
    -> SignedCommitment
    -> VerificationRes
verifySignedCommitment epoch sc@(_, comm, _) = do
    verifyGeneric
        [ ( verifyCommitmentSignature epoch sc
          , "commitment has bad signature (e. g. for wrong epoch)")
        , ( verifyCommitment comm
          , "commitment itself is bad (e. g. no shares")
        ]

-- CHECK: @verifyOpening
-- | Verify that Secret provided with Opening corresponds to given commitment.
--
-- #verifySecretProof
verifyOpening :: Commitment -> Opening -> Bool
verifyOpening Commitment {..} (Opening secret) = fromMaybe False $
    verifySecret thr commProof <$> (rightToMaybe . fromBinary) secret
  where
    thr = vssThreshold $ sum (HM.map length commShares)

-- CHECK: @checkCertTTL
-- | Check that the VSS certificate has valid TTL: i. e. it is in
-- '[vssMinTTL, vssMaxTTL]'.
checkCertTTL :: HasProtocolConstants => EpochIndex -> VssCertificate -> Bool
checkCertTTL curEpochIndex vc =
    expiryEpoch + 1 >= vssMinTTL + curEpochIndex &&
    expiryEpoch < vssMaxTTL + curEpochIndex
  where
    expiryEpoch = vcExpiryEpoch vc

----------------------------------------------------------------------------
-- Payload and proof
----------------------------------------------------------------------------

-- | Removes parts of payload so its binary representation length
-- fits into passed limit. If limit is too low (0), we can return
-- 'Nothing'.
stripSscPayload :: HasProtocolConstants => Byte -> SscPayload -> Maybe SscPayload
stripSscPayload lim payload | biSize payload <= lim = Just payload
stripSscPayload lim payload = case payload of
    (CertificatesPayload vssmap) ->
        CertificatesPayload <$> stripVss lim vssmap
    (CommitmentsPayload (getCommitmentsMap -> comms0) certs0) -> do
        let certs = stripVss limCerts certs0
        let comms = stripHashMap (lim - biSize certs) comms0
        CommitmentsPayload <$> (mkCommitmentsMapUnsafe <$> comms) <*> certs
    (OpeningsPayload openings0 certs0) -> do
        let certs = stripVss limCerts certs0
        let openings = stripHashMap (lim - biSize certs) openings0
        OpeningsPayload <$> openings <*> certs
    (SharesPayload shares0 certs0) -> do
        let certs = stripVss limCerts certs0
        let shares = stripHashMap (lim - biSize certs) shares0
        SharesPayload <$> shares <*> certs
  where
    limCerts = lim `div` 3 -- certificates are 1/3 less important than everything else
                           -- this is a random choice in fact
    -- Using 'UnsafeVssCertificatesMap' is safe here because if the original
    -- map is okay, a subset of the original map is okay too.
    stripVss l = fmap UnsafeVssCertificatesMap .
                 stripHashMap l .
                 getVssCertificatesMap

-- | Default SSC payload depending on local slot index.
defaultSscPayload :: HasProtocolConstants => LocalSlotIndex -> SscPayload
defaultSscPayload lsi
    | isCommitmentIdx lsi = CommitmentsPayload mempty mempty
    | isOpeningIdx lsi = OpeningsPayload mempty mempty
    | isSharesIdx lsi = SharesPayload mempty mempty
    | otherwise = CertificatesPayload mempty
