{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Core functions from SSC.

module Pos.Chain.Ssc.Base
       (
         -- * Helpers
         isCommitmentId
       , isCommitmentIdx
       , isOpeningId
       , isOpeningIdx
       , isSharesId
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

import qualified Data.HashMap.Strict as HM
import           Data.Ix (inRange)
import           Serokell.Data.Memory.Units (Byte)
import           Serokell.Util (VerificationRes, verifyGeneric)

import           Pos.Binary.Class (biSize, fromBinary)
import           Pos.Chain.Ssc.Commitment (Commitment (..), SignedCommitment)
import           Pos.Chain.Ssc.CommitmentsMap
                     (CommitmentsMap (getCommitmentsMap),
                     mkCommitmentsMapUnsafe)
import           Pos.Chain.Ssc.Opening (Opening (..))
import           Pos.Chain.Ssc.Payload (SscPayload (..))
import           Pos.Chain.Ssc.VssCertificate (VssCertificate (vcExpiryEpoch))
import           Pos.Chain.Ssc.VssCertificatesMap (VssCertificatesMap (..))
import           Pos.Core (BlockCount, EpochIndex (..), LocalSlotIndex,
                     SharedSeed (..), SlotCount, SlotId (..), StakeholderId,
                     addressHash, unsafeMkLocalSlotIndex)
import           Pos.Core.Limits (stripHashMap)
import           Pos.Core.ProtocolConstants (ProtocolConstants (..),
                     kEpochSlots, kSlotSecurityParam, vssMaxTTL, vssMinTTL)
import           Pos.Crypto (ProtocolMagic, Secret, SecretKey,
                     SignTag (SignCommitment), Threshold, checkSig,
                     getDhSecret, secretToDhSecret, sign, toPublic,
                     verifySecret)

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

-- | Make signed commitment from commitment and epoch index using secret key.
mkSignedCommitment
    :: ProtocolMagic -> SecretKey -> EpochIndex -> Commitment -> SignedCommitment
mkSignedCommitment pm sk i c = (toPublic sk, c, sign pm SignCommitment sk (i, c))

toLocalSlotIndex :: SlotCount -> SlotCount -> LocalSlotIndex
toLocalSlotIndex epochSlots = unsafeMkLocalSlotIndex epochSlots . fromIntegral

inLocalSlotIndexRange
    :: SlotCount -> (SlotCount, SlotCount) -> LocalSlotIndex -> Bool
inLocalSlotIndexRange epochSlots (lo, hi) =
    inRange (toLocalSlotIndex epochSlots lo, toLocalSlotIndex epochSlots hi)

isCommitmentIdx :: BlockCount -> LocalSlotIndex -> Bool
isCommitmentIdx k =
    inLocalSlotIndexRange (kEpochSlots k) (0, kSlotSecurityParam k - 1)

isOpeningIdx :: BlockCount -> LocalSlotIndex -> Bool
isOpeningIdx k = inLocalSlotIndexRange
    (kEpochSlots k)
    (2 * kSlotSecurityParam k, 3 * kSlotSecurityParam k - 1)

isSharesIdx :: BlockCount -> LocalSlotIndex -> Bool
isSharesIdx k = inLocalSlotIndexRange
    (kEpochSlots k)
    (4 * kSlotSecurityParam k, 5 * kSlotSecurityParam k - 1)

isCommitmentId :: BlockCount -> SlotId -> Bool
isCommitmentId k = isCommitmentIdx k . siSlot

isOpeningId :: BlockCount -> SlotId -> Bool
isOpeningId k = isOpeningIdx k . siSlot

isSharesId :: BlockCount -> SlotId -> Bool
isSharesId k = isSharesIdx k . siSlot

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
    :: ProtocolMagic
    -> EpochIndex
    -> SignedCommitment
    -> Bool
verifyCommitmentSignature pm epoch (pk, comm, commSig) =
    checkSig pm SignCommitment pk (epoch, comm) commSig

-- CHECK: @verifySignedCommitment
-- | Verify SignedCommitment using public key and epoch index.
--
-- #verifyCommitmentSignature
-- #verifyCommitment
verifySignedCommitment
    :: ProtocolMagic
    -> EpochIndex
    -> SignedCommitment
    -> VerificationRes
verifySignedCommitment pm epoch sc@(_, comm, _) = do
    verifyGeneric
        [ ( verifyCommitmentSignature pm epoch sc
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
checkCertTTL :: ProtocolConstants -> EpochIndex -> VssCertificate -> Bool
checkCertTTL pc curEpochIndex vc =
    expiryEpoch + 1 >= vssMinTTL pc + curEpochIndex &&
    expiryEpoch < vssMaxTTL pc + curEpochIndex
  where
    expiryEpoch = vcExpiryEpoch vc

----------------------------------------------------------------------------
-- Payload and proof
----------------------------------------------------------------------------

-- | Removes parts of payload so its binary representation length
-- fits into passed limit. If limit is too low (0), we can return
-- 'Nothing'.
stripSscPayload :: Byte -> SscPayload -> Maybe SscPayload
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
defaultSscPayload :: BlockCount -> LocalSlotIndex -> SscPayload
defaultSscPayload k lsi
    | isCommitmentIdx k lsi = CommitmentsPayload mempty mempty
    | isOpeningIdx k lsi = OpeningsPayload mempty mempty
    | isSharesIdx k lsi = SharesPayload mempty mempty
    | otherwise = CertificatesPayload mempty
