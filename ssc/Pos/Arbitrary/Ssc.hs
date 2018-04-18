-- | Arbitrary instances and generators for SSC types.

module Pos.Arbitrary.Ssc
       ( SscPayloadDependsOnSlot (..)
       , BadCommAndOpening (..)
       , BadSignedCommitment (..)
       , CommitmentOpening (..)

       , commitmentMapEpochGen
       , vssCertificateEpochGen
       , genCommitmentsMap
       , genSignedCommitment
       , genVssCertificatesMap
       , genSscPayload
       , genSscPayloadForSlot
       ) where

import           Universum

import qualified Data.List.NonEmpty as NE
import           Test.QuickCheck (Arbitrary (..), Gen, choose, elements, listOf, oneof)
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary, genericShrink)

import           Pos.Arbitrary.Core (genVssCertificate)
import           Pos.Arbitrary.Core.Unsafe ()
import           Pos.Binary.Ssc ()
import           Pos.Communication.Types.Relay (DataMsg (..))
import           Pos.Core (EpochIndex, SlotId (..), VssCertificate (..),
                           VssCertificatesMap, mkVssCertificate, mkVssCertificatesMapLossy)
import           Pos.Core.ProtocolConstants (ProtocolConstants (..), VssMinTTL (..),
                                             VssMaxTTL (..))
import           Pos.Core.Configuration (HasProtocolConstants, protocolConstants)
import           Pos.Core.Ssc (Commitment (..), CommitmentsMap, Opening (..), SignedCommitment,
                               SscPayload (..), SscProof (..), mkCommitmentsMap)
import           Pos.Crypto (ProtocolMagic, SecretKey, deterministic, randomNumberInRange,
                             toVssPublicKey, vssKeyGen)
import           Pos.Crypto.Configuration (HasProtocolMagic, protocolMagic)
import           Pos.Ssc.Base (genCommitmentAndOpening, isCommitmentIdExplicit,
                               isOpeningIdExplicit, isSharesIdExplicit,
                               mkSignedCommitment)
import           Pos.Ssc.Message (MCCommitment (..), MCOpening (..), MCShares (..),
                                  MCVssCertificate (..), SscTag (..))
import           Pos.Ssc.Toss.Types (TossModifier (..))
import           Pos.Ssc.Types (SscGlobalState (..), SscSecretStorage (..))
import           Pos.Ssc.VssCertData (VssCertData (..))
import           Pos.Util.QuickCheck.Arbitrary (Nonrepeating (..), sublistN)

import           Test.Pos.Crypto.Arbitrary (genSignature)

----------------------------------------------------------------------------
-- Types
----------------------------------------------------------------------------

newtype SscPayloadDependsOnSlot = SscPayloadDependsOnSlot
    { genPayloadDependsOnSlot :: SlotId -> Gen SscPayload
    } deriving Generic

----------------------------------------------------------------------------
-- Core
----------------------------------------------------------------------------

-- | Wrapper over 'SignedCommitment'. Creates an invalid SignedCommitment
-- w.r.t. 'verifyCommitmentSignature'.
newtype BadSignedCommitment = BadSignedComm
    { getBadSignedC :: SignedCommitment
    } deriving (Generic, Show, Eq)

instance HasProtocolMagic => Arbitrary BadSignedCommitment where
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

-- | A 'CommitmentsMap' is made from a '[SignedCommitment]', where
-- 'SignedCommitment' is a type alias for
-- '(PublicKey, Commitment, CommitmentSignature)'. We need a 'ProtocolMagic'
-- to make the 'CommitmentSignature ~ Signature (EpochIndex, Commitment)`
genCommitmentsMap :: ProtocolMagic -> Gen CommitmentsMap
genCommitmentsMap pm = mkCommitmentsMap <$> listOf (genSignedCommitment pm)

-- | 'SignedCommitment = (PublicKey, Commitment, CommitmentSignature)'
genSignedCommitment :: ProtocolMagic -> Gen SignedCommitment
genSignedCommitment pm = (,,) <$> arbitrary <*> arbitrary <*> genSignature pm arbitrary

instance HasProtocolMagic => Arbitrary CommitmentsMap where
    arbitrary = genCommitmentsMap protocolMagic
    shrink = genericShrink

-- | Generates commitment map having commitments from given epoch.
commitmentMapEpochGen :: ProtocolMagic -> EpochIndex -> Gen CommitmentsMap
commitmentMapEpochGen pm i = do
    (coms :: [(SecretKey, Commitment)]) <- listOf $ (,) <$> arbitrary <*> arbitrary
    pure $ mkCommitmentsMap $
        map (\(sk,com) -> mkSignedCommitment pm sk i com) coms

instance Arbitrary Opening where
    arbitrary = coOpening <$> arbitrary

-- | For given epoch @e@ enerates vss certificate having epoch in
-- range @[e+vssMin,e+vssMax)@.
vssCertificateEpochGen :: ProtocolMagic -> ProtocolConstants -> EpochIndex -> Gen VssCertificate
vssCertificateEpochGen pm pc x = do
    e <- choose (getVssMinTTL (pcVssMinTTL pc), getVssMaxTTL (pcVssMaxTTL pc) - 1)
    mkVssCertificate pm <$> arbitrary <*> arbitrary <*> pure (fromIntegral e + x)

----------------------------------------------------------------------------
-- SSC types
----------------------------------------------------------------------------

instance (HasProtocolConstants, HasProtocolMagic) => Arbitrary SscProof where
    arbitrary = genericArbitrary
    shrink = genericShrink

genSscPayload :: ProtocolMagic -> Gen SscPayload
genSscPayload pm =
    oneof
        [ CommitmentsPayload <$> genCommitmentsMap pm <*> genVssCertificatesMap pm
        , OpeningsPayload <$> arbitrary <*> genVssCertificatesMap pm
        , SharesPayload <$> arbitrary <*> genVssCertificatesMap pm
        , CertificatesPayload <$> genVssCertificatesMap pm
        ]

instance (HasProtocolMagic) => Arbitrary SscPayload where
    arbitrary = genSscPayload protocolMagic
    shrink = genericShrink

-- | We need the 'ProtocolConstants' because they give meaning to 'SlotId'.
genSscPayloadForSlot :: ProtocolMagic -> ProtocolConstants -> SlotId -> Gen SscPayload
genSscPayloadForSlot pm pc slot
    | isCommitmentIdExplicit pc slot =
        CommitmentsPayload <$> (genCommitments slot) <*> (genVssCerts slot)
    | isOpeningIdExplicit pc slot =
        OpeningsPayload <$> arbitrary <*> (genVssCerts slot)
    | isSharesIdExplicit pc slot =
        SharesPayload <$> arbitrary <*> (genVssCerts slot)
    | otherwise =
        CertificatesPayload <$> (genVssCerts slot)
  where
    genCommitments slot' =
        mkCommitmentsMap .
        map (genValidComm slot') <$>
        arbitrary
    genValidComm SlotId{..} (sk, c) = mkSignedCommitment pm sk siEpoch c

    genVssCerts slot' =
        mkVssCertificatesMapLossy .
        map (genValidCert slot') <$>
        arbitrary
    genValidCert SlotId{..} (sk, pk) = mkVssCertificate pm sk pk $ siEpoch + 5

instance (HasProtocolConstants, HasProtocolMagic) => Arbitrary SscPayloadDependsOnSlot where
    arbitrary = pure $ SscPayloadDependsOnSlot (genSscPayloadForSlot protocolMagic protocolConstants)

genVssCertificatesMap :: ProtocolMagic -> Gen VssCertificatesMap
genVssCertificatesMap pm = do
    certs <- listOf (genVssCertificate pm)
    pure $ mkVssCertificatesMapLossy certs

instance HasProtocolMagic => Arbitrary VssCertificatesMap where
    arbitrary = genVssCertificatesMap protocolMagic
    shrink = genericShrink

instance (HasProtocolConstants, HasProtocolMagic) => Arbitrary VssCertData where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance (HasProtocolConstants, HasProtocolMagic) => Arbitrary SscGlobalState where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance HasProtocolMagic => Arbitrary SscSecretStorage where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance (HasProtocolConstants, HasProtocolMagic) => Arbitrary TossModifier where
    arbitrary = genericArbitrary
    shrink = genericShrink

------------------------------------------------------------------------------------------
-- Message types
------------------------------------------------------------------------------------------

instance Arbitrary SscTag where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance HasProtocolMagic => Arbitrary MCCommitment where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary MCOpening where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary MCShares where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance (HasProtocolConstants, HasProtocolMagic) => Arbitrary MCVssCertificate where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance HasProtocolMagic => Arbitrary (DataMsg MCCommitment) where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary (DataMsg MCOpening) where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary (DataMsg MCShares) where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance (HasProtocolConstants, HasProtocolMagic) => Arbitrary (DataMsg MCVssCertificate) where
    arbitrary = genericArbitrary
    shrink = genericShrink
