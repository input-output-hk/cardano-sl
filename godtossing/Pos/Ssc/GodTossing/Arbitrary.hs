{-# LANGUAGE ScopedTypeVariables #-}

-- | Arbitrary instances and generators for GodTossing types.

module Pos.Ssc.GodTossing.Arbitrary
       ( BadCommAndOpening (..)
       , BadCommitment (..)
       , BadSignedCommitment (..)
       , CommitmentOpening (..)
       , commitmentMapEpochGen
       , vssCertificateEpochGen
       ) where

import           Universum

import qualified Data.HashMap.Strict               as HM
import           Test.QuickCheck                   (Arbitrary (..), Gen, choose, elements,
                                                    listOf, oneof)
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary, genericShrink)

import           Pos.Binary.Class                  (asBinary)
import           Pos.Binary.GodTossing             ()
import           Pos.Communication.Types.Relay     (DataMsg (..))
import           Pos.Core                          (EpochIndex, SlotId (..), addressHash,
                                                    addressHash)
import           Pos.Core.Arbitrary.Unsafe         ()
import           Pos.Crypto                        (SecretKey, deterministicVssKeyGen,
                                                    toVssPublicKey)
import           Pos.Ssc.Arbitrary                 (SscPayloadDependsOnSlot (..))
import           Pos.Ssc.GodTossing.Constants      (vssMaxTTL, vssMinTTL)
import           Pos.Ssc.GodTossing.Core           (Commitment (..), CommitmentsMap,
                                                    GtPayload (..), GtProof (..),
                                                    Opening (..), Opening (..),
                                                    SignedCommitment, VssCertificate (..),
                                                    genCommitmentAndOpening,
                                                    isCommitmentId, isOpeningId,
                                                    isSharesId, mkCommitmentsMap,
                                                    mkCommitmentsMap, mkSignedCommitment,
                                                    mkVssCertificate)
import qualified Pos.Ssc.GodTossing.Genesis.Types  as G
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

-- | Wrapper over 'Commitment'. Creates an invalid Commitment w.r.t. 'verifyCommitment'.
newtype BadCommitment = BadComm
    { getBadComm :: Commitment
    } deriving (Generic, Show, Eq)

instance Arbitrary BadCommitment where
    arbitrary = BadComm <$> do
        Commitment <$> arbitrary <*> arbitrary <*> arbitrary
    shrink = genericShrink

-- | Wrapper over 'SignedCommitment'. Creates an invalid SignedCommitment w.r.t.
-- 'verifyCommitmentSignature'.
newtype BadSignedCommitment = BadSignedComm
    { getBadSignedC :: SignedCommitment
    } deriving (Generic, Show, Eq)

instance Arbitrary BadSignedCommitment where
    arbitrary = BadSignedComm <$> do
        pk <- arbitrary
        sig <- arbitrary
        badComm <- getBadComm <$> (arbitrary :: Gen BadCommitment)
        return (pk, badComm, sig)
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
        badComm <- getBadComm <$> arbitrary
        opening <- arbitrary
        return $ BadCommAndOpening (badComm, opening)
    shrink = genericShrink

-- | Generate 50 commitment/opening pairs in advance
-- (see `Pos.Crypto.Arbitrary` for explanations)
commitmentsAndOpenings :: [CommitmentOpening]
commitmentsAndOpenings =
    map (uncurry CommitmentOpening) $
    unsafeMakePool "[generating Commitments and Openings for tests...]" 50 $
       genCommitmentAndOpening 1 (one (asBinary vssPk))
  where
    vssPk = toVssPublicKey $ deterministicVssKeyGen "ababahalamaha"
{-# NOINLINE commitmentsAndOpenings #-}

instance Arbitrary CommitmentOpening where
    arbitrary = elements commitmentsAndOpenings
    shrink = genericShrink

instance Nonrepeating CommitmentOpening where
    nonrepeating n = sublistN n commitmentsAndOpenings

instance Arbitrary Commitment where
    arbitrary = coCommitment <$> arbitrary
    shrink Commitment {..} = [ Commitment { commShares = shrunkShares, .. }
                             | shrunkShares <- filter (not . null) $ shrink commShares
                             ]

instance Arbitrary CommitmentsMap where
    arbitrary = mkCommitmentsMap <$> arbitrary
    shrink = genericShrink

-- | Generates commitment map having commitments from given epoch.
commitmentMapEpochGen :: EpochIndex -> Gen CommitmentsMap
commitmentMapEpochGen i = do
    (coms :: [(SecretKey, Commitment)]) <- listOf $ (,) <$> arbitrary <*> arbitrary
    pure $ mkCommitmentsMap $
        map (\(sk,com) -> mkSignedCommitment sk i com) coms

instance Arbitrary Opening where
    arbitrary = coOpening <$> arbitrary

instance Arbitrary VssCertificate where
    arbitrary = mkVssCertificate <$> arbitrary <*> arbitrary <*> arbitrary

-- | For given epoch @e@ enerates vss certificate having epoch in
-- range @[e+vssMin,e+vssMax)@.
vssCertificateEpochGen :: EpochIndex -> Gen VssCertificate
vssCertificateEpochGen x = do
    e <- choose (vssMinTTL, vssMaxTTL-1)
    mkVssCertificate <$> arbitrary <*> arbitrary <*> pure (e + x)

----------------------------------------------------------------------------
-- Gt (God Tossing) types
----------------------------------------------------------------------------

instance Arbitrary GtProof where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary GtPayload where
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

instance Arbitrary (SscPayloadDependsOnSlot SscGodTossing) where
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

instance Arbitrary VssCertData where
    arbitrary = makeSmall genericArbitrary
    shrink = genericShrink

instance Arbitrary GtGlobalState where
    arbitrary = makeSmall genericArbitrary
    shrink = genericShrink

instance Arbitrary GtSecretStorage where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary TossModifier where
    arbitrary = makeSmall genericArbitrary
    shrink = genericShrink

------------------------------------------------------------------------------------------
-- Message types
------------------------------------------------------------------------------------------

instance Arbitrary GtTag where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary MCCommitment where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary MCOpening where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary MCShares where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary MCVssCertificate where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary (DataMsg MCCommitment) where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary (DataMsg MCOpening) where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary (DataMsg MCShares) where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary (DataMsg MCVssCertificate) where
    arbitrary = genericArbitrary
    shrink = genericShrink

----------------------------------------------------------------------------
-- Arbitrary types from 'Pos.Ssc.GodTossing.Genesis.Types'
----------------------------------------------------------------------------

instance Arbitrary G.GenesisGtData where
    arbitrary = genericArbitrary
    shrink = genericShrink
