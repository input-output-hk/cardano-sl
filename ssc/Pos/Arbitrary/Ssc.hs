-- | Arbitrary instances and generators for SSC types.

module Pos.Arbitrary.Ssc
       ( {-SscPayloadDependsOnSlot (..)
       , -}
         BadCommAndOpening (..)
       , BadSignedCommitment (..)
       , CommitmentOpening (..)
       , commitmentMapEpochGen
       , vssCertificateEpochGen
       ) where

import           Universum

import qualified Data.List.NonEmpty as NE
import           Test.QuickCheck (Arbitrary (..), Gen, choose, elements, listOf, oneof)
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary, genericShrink)

import           Pos.Arbitrary.Core.Unsafe ()
import           Pos.Binary.Ssc ()
import           Pos.Communication.Types.Relay (DataMsg (..))
import           Pos.Core (EpochIndex (..), VssCertificate (..),
                           VssCertificatesMap, mkVssCertificate, mkVssCertificatesMapLossy,
                           VssMaxTTL (..), VssMinTTL (..))
import           Pos.Core.Ssc (Commitment (..), CommitmentsMap, Opening (..), SignedCommitment,
                               SscPayload (..), SscProof (..), mkCommitmentsMap)
import           Pos.Crypto (ProtocolMagic, SecretKey, deterministic, randomNumberInRange,
                             toVssPublicKey, vssKeyGen)
import           Pos.Ssc.Base (genCommitmentAndOpening, mkSignedCommitment)
import           Pos.Ssc.Message (MCCommitment (..), MCOpening (..), MCShares (..),
                                  MCVssCertificate (..), SscTag (..))
import           Pos.Ssc.Toss.Types (TossModifier (..))
import           Pos.Ssc.Types (SscGlobalState (..), SscSecretStorage (..))
import           Pos.Ssc.VssCertData (VssCertData (..))
import           Pos.Util.QuickCheck.Arbitrary (Nonrepeating (..), sublistN)

----------------------------------------------------------------------------
-- Types
----------------------------------------------------------------------------

{-
newtype SscPayloadDependsOnSlot = SscPayloadDependsOnSlot
    { genPayloadDependsOnSlot :: SlotId -> Gen SscPayload
    } deriving Generic
-}

----------------------------------------------------------------------------
-- Core
----------------------------------------------------------------------------

-- | Wrapper over 'SignedCommitment'. Creates an invalid SignedCommitment
-- w.r.t. 'verifyCommitmentSignature'.
newtype BadSignedCommitment = BadSignedComm
    { getBadSignedC :: SignedCommitment
    } deriving (Generic, Show, Eq)

instance Arbitrary ProtocolMagic => Arbitrary BadSignedCommitment where
    arbitrary = BadSignedComm <$> do
        pk <- arbitrary
        sig <- arbitrary
        comm <- Commitment <$> arbitrary <*> arbitrary
        return (pk, comm, sig)
    shrink = genericShrink

-- | Pair of 'Commitment' and 'Opening'.
data CommitmentOpening = CommitmentOpening
    { coCommitment :: !Commitment
    , coOpening    :: !Opening
    } deriving (Generic, Show)

-- | Wrapper over '(Commitment, Opening)'. Creates an invalid pair of a Commitment and an
-- Opening w.r.t. 'verifyOpening'.
data BadCommAndOpening = BadCommAndOpening
    { getBadCAndO :: (Commitment, Opening)
    } deriving (Generic, Show, Eq)

instance Arbitrary BadCommAndOpening where
    arbitrary = do
        badComm <- Commitment <$> arbitrary <*> arbitrary
        opening <- arbitrary
        return $ BadCommAndOpening (badComm, opening)
    shrink = genericShrink

-- | Generate 50 commitment/opening pairs for tests.
commitmentsAndOpenings :: [CommitmentOpening]
commitmentsAndOpenings =
    map (uncurry CommitmentOpening) $
    deterministic "commitmentsAndOpenings" $ replicateM 50 $ do
      t <- randomNumberInRange 3 10
      n <- randomNumberInRange (t*2-1) (t*2)
      vssKeys <- replicateM (fromInteger n) $ toVssPublicKey <$> vssKeyGen
      genCommitmentAndOpening (fromIntegral t) (NE.fromList vssKeys)

instance Arbitrary CommitmentOpening where
    arbitrary = elements commitmentsAndOpenings
    shrink = genericShrink

instance Nonrepeating CommitmentOpening where
    nonrepeating n = sublistN n commitmentsAndOpenings

instance Arbitrary Commitment where
    arbitrary = coCommitment <$> arbitrary
    -- No other field is shrunk in the implementation of 'shrink'
    -- for this type because:
    -- 1. The datatype's invariant cannot be broken
    -- 2. The cryptographic datatypes used here don't have 'shrink' implemented
    shrink Commitment {..} = [ Commitment { commShares = shrunkShares, .. }
                             | shrunkShares <- filter (not . null) $ shrink commShares
                             ]

instance Arbitrary ProtocolMagic => Arbitrary CommitmentsMap where
    arbitrary = mkCommitmentsMap <$> arbitrary
    shrink = genericShrink

-- | Generates commitment map having commitments from given epoch.
commitmentMapEpochGen :: Arbitrary ProtocolMagic => EpochIndex -> Gen CommitmentsMap
commitmentMapEpochGen i = do
    pm <- arbitrary
    (coms :: [(SecretKey, Commitment)]) <- listOf $ (,) <$> arbitrary <*> arbitrary
    pure $ mkCommitmentsMap $
        map (\(sk,com) -> mkSignedCommitment pm sk i com) coms

instance Arbitrary Opening where
    arbitrary = coOpening <$> arbitrary

-- | For given epoch @e@ generates vss certificate having epoch in
-- range @[e+vssMin,e+vssMax)@.
vssCertificateEpochGen
    :: ( Arbitrary ProtocolMagic, Arbitrary VssMaxTTL, Arbitrary VssMinTTL )
    => EpochIndex
    -> Gen VssCertificate
vssCertificateEpochGen x = do
    mini <- getVssMinTTL <$> arbitrary
    maxi <- getVssMaxTTL <$> arbitrary
    e <- EpochIndex <$> choose (fromIntegral mini, fromIntegral (maxi - 1))
    mkVssCertificate <$> arbitrary <*> arbitrary <*> arbitrary <*> pure (e + x)

----------------------------------------------------------------------------
-- SSC types
----------------------------------------------------------------------------

instance Arbitrary SscProof where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary ProtocolMagic => Arbitrary SscPayload where
    arbitrary =
        oneof
            [ CommitmentsPayload <$> arbitrary <*> arbitrary
            , OpeningsPayload <$> arbitrary <*> arbitrary
            , SharesPayload <$> arbitrary <*> arbitrary
            , CertificatesPayload <$> arbitrary
            ]
    shrink = genericShrink

{-
instance (Arbitrary ProtocolMagic) => Arbitrary SscPayloadDependsOnSlot where
    arbitrary = pure $ SscPayloadDependsOnSlot payloadGen
      where
        payloadGen slot
            | isCommitmentId slot =
                CommitmentsPayload <$> (genCommitments slot) <*> (genVssCerts slot)
            | isOpeningId slot =
                OpeningsPayload <$> arbitrary <*> (genVssCerts slot)
            | isSharesId slot =
                SharesPayload <$> arbitrary <*> (genVssCerts slot)
            | otherwise =
                CertificatesPayload <$> (genVssCerts slot)
        genCommitments slot = do
            pm <- arbitrary
            rest <- arbitrary
            pure (mkCommitmentsMap (map (genValidComm slot pm) rest))
        genValidComm SlotId{..} pm (sk, c) = mkSignedCommitment pm sk siEpoch c

        genVssCerts slot = do
            pm <- arbitrary
            rest <- arbitrary
            pure (mkVssCertificatesMapLossy (map (genValidCert slot pm) rest))
        genValidCert SlotId{..} pm (sk, pk) = mkVssCertificate pm sk pk $ siEpoch + 5
-}

instance Arbitrary ProtocolMagic => Arbitrary VssCertificatesMap where
    arbitrary = do
        certs <- arbitrary
        pure $ mkVssCertificatesMapLossy certs
    shrink = genericShrink

instance Arbitrary ProtocolMagic => Arbitrary VssCertData where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary ProtocolMagic => Arbitrary SscGlobalState where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary ProtocolMagic => Arbitrary SscSecretStorage where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary ProtocolMagic => Arbitrary TossModifier where
    arbitrary = genericArbitrary
    shrink = genericShrink

------------------------------------------------------------------------------------------
-- Message types
------------------------------------------------------------------------------------------

instance Arbitrary SscTag where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary ProtocolMagic => Arbitrary MCCommitment where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary MCOpening where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary MCShares where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary ProtocolMagic => Arbitrary MCVssCertificate where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary ProtocolMagic => Arbitrary (DataMsg MCCommitment) where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary (DataMsg MCOpening) where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary (DataMsg MCShares) where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary ProtocolMagic => Arbitrary (DataMsg MCVssCertificate) where
    arbitrary = genericArbitrary
    shrink = genericShrink
