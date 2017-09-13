-- | Arbitrary instances and generators for GodTossing types.

module Pos.Arbitrary.Ssc.GodTossing
       ( BadCommAndOpening (..)
       , BadSignedCommitment (..)
       , CommitmentOpening (..)
       , commitmentMapEpochGen
       , vssCertificateEpochGen
       ) where

import           Universum

import qualified Data.HashMap.Strict               as HM
import qualified Data.List.NonEmpty                as NE
import qualified System.Random                     as R
import           Test.QuickCheck                   (Arbitrary (..), Gen, choose, elements,
                                                    listOf, oneof)
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary, genericShrink)

import           Pos.Arbitrary.Core.Unsafe         ()
import           Pos.Arbitrary.Ssc                 (SscPayloadDependsOnSlot (..))
import           Pos.Binary.Class                  (asBinary)
import           Pos.Binary.GodTossing             ()
import           Pos.Communication.Types.Relay     (DataMsg (..))
import           Pos.Core                          (EpochIndex, HasConfiguration,
                                                    SlotId (..), VssCertificate (..),
                                                    addressHash, mkVssCertificate,
                                                    vssMaxTTL, vssMinTTL)
import           Pos.Crypto                        (SecretKey, toVssPublicKey, vssKeyGen)
import           Pos.Ssc.GodTossing.Core           (Commitment (..), CommitmentsMap,
                                                    GtPayload (..), GtProof (..),
                                                    Opening (..), SignedCommitment,
                                                    genCommitmentAndOpening,
                                                    isCommitmentId, isOpeningId,
                                                    isSharesId, mkCommitmentsMap,
                                                    mkCommitmentsMap, mkSignedCommitment)
import           Pos.Ssc.GodTossing.Toss.Types     (TossModifier (..))
import           Pos.Ssc.GodTossing.Type           (SscGodTossing)
import           Pos.Ssc.GodTossing.Types.Message  (GtTag (..), MCCommitment (..),
                                                    MCOpening (..), MCShares (..),
                                                    MCVssCertificate (..))
import           Pos.Ssc.GodTossing.Types.Types    (GtGlobalState (..),
                                                    GtSecretStorage (..))
import           Pos.Ssc.GodTossing.VssCertData    (VssCertData (..))
import           Pos.Util.Arbitrary                (Nonrepeating (..), makeSmall,
                                                    sublistN, unsafeMakePool)

----------------------------------------------------------------------------
-- Core
----------------------------------------------------------------------------

-- | Wrapper over 'SignedCommitment'. Creates an invalid SignedCommitment
-- w.r.t. 'verifyCommitmentSignature'.
newtype BadSignedCommitment = BadSignedComm
    { getBadSignedC :: SignedCommitment
    } deriving (Generic, Show, Eq)

instance HasConfiguration => Arbitrary BadSignedCommitment where
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

-- | Generate 50 commitment/opening pairs in advance
-- (see `Pos.Crypto.Arbitrary` for explanations)
commitmentsAndOpenings :: [CommitmentOpening]
commitmentsAndOpenings =
    map (uncurry CommitmentOpening) $
    unsafeMakePool "[generating Commitments and Openings for tests...]" 50 $ do
      t <- R.randomRIO (3, 10)
      n <- R.randomRIO (t*2-1, t*2)
      vssKeys <- replicateM n $ toVssPublicKey <$> vssKeyGen
      genCommitmentAndOpening (fromIntegral t)
          (NE.fromList (map asBinary vssKeys))
{-# NOINLINE commitmentsAndOpenings #-}

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

instance HasConfiguration => Arbitrary CommitmentsMap where
    arbitrary = mkCommitmentsMap <$> arbitrary
    shrink = genericShrink

-- | Generates commitment map having commitments from given epoch.
commitmentMapEpochGen :: HasConfiguration => EpochIndex -> Gen CommitmentsMap
commitmentMapEpochGen i = do
    (coms :: [(SecretKey, Commitment)]) <- listOf $ (,) <$> arbitrary <*> arbitrary
    pure $ mkCommitmentsMap $
        map (\(sk,com) -> mkSignedCommitment sk i com) coms

instance Arbitrary Opening where
    arbitrary = coOpening <$> arbitrary

-- | For given epoch @e@ enerates vss certificate having epoch in
-- range @[e+vssMin,e+vssMax)@.
vssCertificateEpochGen :: (HasConfiguration) => EpochIndex -> Gen VssCertificate
vssCertificateEpochGen x = do
    e <- choose (vssMinTTL, vssMaxTTL-1)
    mkVssCertificate <$> arbitrary <*> arbitrary <*> pure (e + x)

----------------------------------------------------------------------------
-- Gt (God Tossing) types
----------------------------------------------------------------------------

instance HasConfiguration => Arbitrary GtProof where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance HasConfiguration => Arbitrary GtPayload where
    arbitrary =
        makeSmall $
        oneof
            [ CommitmentsPayload <$> arbitrary <*> genVssCerts
            , OpeningsPayload <$> arbitrary <*> genVssCerts
            , SharesPayload <$> arbitrary <*> genVssCerts
            , CertificatesPayload <$> genVssCerts
            ]
      where
        genVssCerts = HM.fromList . map toCertPair <$> arbitrary
        toCertPair vc = (addressHash $ vcSigningKey vc, vc)
    shrink = genericShrink

instance HasConfiguration => Arbitrary (SscPayloadDependsOnSlot SscGodTossing) where
    arbitrary = pure $ SscPayloadDependsOnSlot payloadGen
      where
        payloadGen slot
            | isCommitmentId slot =
                makeSmall $ CommitmentsPayload <$> (genCommitments slot) <*> (genVssCerts slot)
            | isOpeningId slot =
                makeSmall $ OpeningsPayload <$> arbitrary <*> (genVssCerts slot)
            | isSharesId slot =
                makeSmall $ SharesPayload <$> arbitrary <*> (genVssCerts slot)
            | otherwise =
                makeSmall $ CertificatesPayload <$> (genVssCerts slot)
        genCommitments slot =
            mkCommitmentsMap .
            map (genValidComm slot) <$>
            arbitrary
        genValidComm SlotId{..} (sk, c) = mkSignedCommitment sk siEpoch c

        genVssCerts slot = HM.fromList . map (toCertPair . genValidCert slot) <$> arbitrary
        toCertPair vc = (addressHash $ vcSigningKey vc, vc)
        genValidCert SlotId{..} (sk, pk) = mkVssCertificate sk pk $ siEpoch + 5

instance HasConfiguration => Arbitrary VssCertData where
    arbitrary = makeSmall genericArbitrary
    shrink = genericShrink

instance HasConfiguration => Arbitrary GtGlobalState where
    arbitrary = makeSmall genericArbitrary
    shrink = genericShrink

instance HasConfiguration => Arbitrary GtSecretStorage where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance HasConfiguration => Arbitrary TossModifier where
    arbitrary = makeSmall genericArbitrary
    shrink = genericShrink

------------------------------------------------------------------------------------------
-- Message types
------------------------------------------------------------------------------------------

instance Arbitrary GtTag where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance HasConfiguration => Arbitrary MCCommitment where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary MCOpening where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary MCShares where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance HasConfiguration => Arbitrary MCVssCertificate where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance HasConfiguration => Arbitrary (DataMsg MCCommitment) where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary (DataMsg MCOpening) where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary (DataMsg MCShares) where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance HasConfiguration => Arbitrary (DataMsg MCVssCertificate) where
    arbitrary = genericArbitrary
    shrink = genericShrink
