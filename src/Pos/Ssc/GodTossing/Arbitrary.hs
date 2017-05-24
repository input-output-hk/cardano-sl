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

import qualified Data.HashMap.Strict              as HM
import           Test.QuickCheck                  (Arbitrary (..), Gen, choose, elements,
                                                   listOf, oneof)

import           Pos.Binary.Class                 (asBinary)
import           Pos.Binary.Ssc                   ()
import           Pos.Communication.Types.Relay    (DataMsg (..))
import           Pos.Constants                    (vssMaxTTL, vssMinTTL)
import           Pos.Core                         (EpochIndex, SlotId (..), addressHash,
                                                   addressHash)
import           Pos.Crypto                       (SecretKey, deterministicVssKeyGen,
                                                   toVssPublicKey)
import           Pos.Ssc.Arbitrary                (SscPayloadDependsOnSlot (..))
import           Pos.Ssc.GodTossing.Core          (Commitment (..), CommitmentsMap,
                                                   GtPayload (..), GtProof (..),
                                                   Opening (..), Opening (..),
                                                   SignedCommitment, VssCertificate (..),
                                                   genCommitmentAndOpening,
                                                   isCommitmentId, isOpeningId,
                                                   isSharesId, mkCommitmentsMap,
                                                   mkCommitmentsMap, mkSignedCommitment,
                                                   mkVssCertificate)
import           Pos.Ssc.GodTossing.Toss.Types    (TossModifier (..))
import           Pos.Ssc.GodTossing.Type          (SscGodTossing)
import           Pos.Ssc.GodTossing.Types.Message (GtTag (..), MCCommitment (..),
                                                   MCOpening (..), MCShares (..),
                                                   MCVssCertificate (..))
import           Pos.Ssc.GodTossing.Types.Types   (GtGlobalState (..),
                                                   GtSecretStorage (..))
import           Pos.Ssc.GodTossing.VssCertData   (VssCertData (..))
import           Pos.Types.Arbitrary.Unsafe       ()
import           Pos.Util.Arbitrary               (Nonrepeating (..), makeSmall, sublistN,
                                                   unsafeMakePool)

----------------------------------------------------------------------------
-- Core
----------------------------------------------------------------------------

-- | Wrapper over 'Commitment'. Creates an invalid Commitment w.r.t. 'verifyCommitment'.
newtype BadCommitment = BadComm
    { getBadComm :: Commitment
    } deriving (Show, Eq)

instance Arbitrary BadCommitment where
    arbitrary = BadComm <$> do
      Commitment <$> arbitrary <*> arbitrary <*> arbitrary

-- | Wrapper over 'SignedCommitment'. Creates an invalid SignedCommitment w.r.t.
-- 'verifyCommitmentSignature'.
newtype BadSignedCommitment = BadSignedComm
    { getBadSignedC :: SignedCommitment
    } deriving (Show, Eq)

instance Arbitrary BadSignedCommitment where
    arbitrary = BadSignedComm <$> do
        pk <- arbitrary
        sig <- arbitrary
        badComm <- getBadComm <$> (arbitrary :: Gen BadCommitment)
        return (pk, badComm, sig)

-- | Pair of 'Commitment' and 'Opening'.
data CommitmentOpening = CommitmentOpening
    { coCommitment :: !Commitment
    , coOpening    :: !Opening
    } deriving Show

-- | Wrapper over '(Commitment, Opening)'. Creates an invalid pair of a Commitment and an
-- Opening w.r.t. 'verifyOpening'.
data BadCommAndOpening = BadCommAndOpening
    { getBadCAndO :: (Commitment, Opening)
    } deriving (Show, Eq)

instance Arbitrary BadCommAndOpening where
    arbitrary = do
        badComm <- getBadComm <$> arbitrary
        opening <- arbitrary
        return $ BadCommAndOpening (badComm, opening)

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

instance Nonrepeating CommitmentOpening where
    nonrepeating n = sublistN n commitmentsAndOpenings

instance Arbitrary Commitment where
    arbitrary = coCommitment <$> arbitrary

instance Arbitrary CommitmentsMap where
    arbitrary = mkCommitmentsMap <$> arbitrary

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
    arbitrary = oneof [
                        CommitmentsProof <$> arbitrary <*> arbitrary
                      , OpeningsProof <$> arbitrary <*> arbitrary
                      , SharesProof <$> arbitrary <*> arbitrary
                      , CertificatesProof <$> arbitrary
                      ]

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
    arbitrary = makeSmall $ VssCertData
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

instance Arbitrary GtGlobalState where
    arbitrary = makeSmall $ GtGlobalState
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

instance Arbitrary GtSecretStorage where
    arbitrary = GtSecretStorage <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary TossModifier where
    arbitrary =
        makeSmall $
        TossModifier <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

------------------------------------------------------------------------------------------
-- Message types
------------------------------------------------------------------------------------------

instance Arbitrary GtTag where
    arbitrary = oneof [ pure CommitmentMsg
                      , pure OpeningMsg
                      , pure SharesMsg
                      , pure VssCertificateMsg
                      ]

instance Arbitrary MCCommitment where
    arbitrary = MCCommitment <$> arbitrary

instance Arbitrary MCOpening where
    arbitrary = MCOpening <$> arbitrary <*> arbitrary

instance Arbitrary MCShares where
    arbitrary = MCShares <$> arbitrary <*> arbitrary

instance Arbitrary MCVssCertificate where
    arbitrary = MCVssCertificate <$> arbitrary

instance Arbitrary (DataMsg MCCommitment) where
    arbitrary = DataMsg <$> arbitrary

instance Arbitrary (DataMsg MCOpening) where
    arbitrary = DataMsg <$> arbitrary

instance Arbitrary (DataMsg MCShares) where
    arbitrary = DataMsg <$> arbitrary

instance Arbitrary (DataMsg MCVssCertificate) where
    arbitrary = DataMsg <$> arbitrary
