{-# LANGUAGE UndecidableInstances #-}

-- | Arbitrary instances for GodTossing types.

module Pos.Ssc.GodTossing.Arbitrary
       ( CommitmentOpening (..)
       ) where

import           Data.List.NonEmpty               (NonEmpty ((:|)))
import           Test.QuickCheck                  (Arbitrary (..), elements, oneof)
import           Universum

import           Pos.Binary.Class                 (Bi)
import           Pos.Crypto                       (deterministicVssKeyGen, toVssPublicKey)
import           Pos.Ssc.GodTossing.Functions     (genCommitmentAndOpening)
import           Pos.Ssc.GodTossing.Secret.Types  (GtSecretStorage (..))
import           Pos.Ssc.GodTossing.Types.Base    (Commitment, Opening,
                                                   VssCertificate (..), mkVssCertificate)
import           Pos.Ssc.GodTossing.Types.Message (DataMsg (..), InvMsg (..), MsgTag (..),
                                                   ReqMsg (..))
import           Pos.Ssc.GodTossing.Types.Types   (GtGlobalState (..), GtPayload (..),
                                                   GtProof (..))
import           Pos.Types.Arbitrary.Unsafe       ()
import           Pos.Util                         (asBinary)
import           Pos.Util.Arbitrary               (Nonrepeating (..), makeSmall, sublistN,
                                                   unsafeMakePool)

-- | Pair of 'Commitment' and 'Opening'.
data CommitmentOpening = CommitmentOpening
    { coCommitment :: !Commitment
    , coOpening    :: !Opening
    } deriving Show

-- | Generate 50 commitment/opening pairs in advance
-- (see `Pos.Crypto.Arbitrary` for explanations)
commitmentsAndOpenings :: [CommitmentOpening]
commitmentsAndOpenings =
    map (uncurry CommitmentOpening) $
    unsafeMakePool "[generating Commitments and Openings for tests...]" 50 $
       genCommitmentAndOpening 1 (asBinary vssPk :| [])
  where
    vssPk = toVssPublicKey $ deterministicVssKeyGen "aaaaaaaaaaaaaaaaaaaaaassss"
{-# NOINLINE commitmentsAndOpenings #-}

instance Arbitrary CommitmentOpening where
    arbitrary = elements commitmentsAndOpenings

instance Nonrepeating CommitmentOpening where
    nonrepeating n = sublistN n commitmentsAndOpenings

instance Arbitrary Commitment where
    arbitrary = coCommitment <$> arbitrary

instance Arbitrary Opening where
    arbitrary = coOpening <$> arbitrary

instance Arbitrary VssCertificate where
    arbitrary = mkVssCertificate <$> arbitrary <*> arbitrary

------------------------------------------------------------------------------------------
-- Gt (God Tossing) types
------------------------------------------------------------------------------------------

instance (Bi Commitment, Bi Opening, Bi VssCertificate) => Arbitrary GtProof where
    arbitrary = oneof [
                        CommitmentsProof <$> arbitrary <*> arbitrary
                      , OpeningsProof <$> arbitrary <*> arbitrary
                      , SharesProof <$> arbitrary <*> arbitrary
                      , CertificatesProof <$> arbitrary
                      ]

instance Bi Commitment => Arbitrary GtPayload where
    arbitrary = makeSmall $ oneof [ CommitmentsPayload <$> arbitrary <*> arbitrary
                                  , OpeningsPayload <$> arbitrary <*> arbitrary
                                  , SharesPayload <$> arbitrary <*> arbitrary
                                  , CertificatesPayload <$> arbitrary
                                  ]

instance Bi Commitment => Arbitrary GtGlobalState where
    arbitrary = makeSmall $ GtGlobalState
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

instance Bi Commitment => Arbitrary GtSecretStorage where
    arbitrary = GtSecretStorage <$> arbitrary <*> arbitrary

------------------------------------------------------------------------------------------
-- Message types
------------------------------------------------------------------------------------------

instance Arbitrary MsgTag where
    arbitrary = oneof [ pure CommitmentMsg
                      , pure OpeningMsg
                      , pure SharesMsg
                      , pure VssCertificateMsg
                      ]

instance Arbitrary InvMsg where
    arbitrary = InvMsg <$> arbitrary <*> arbitrary

instance Arbitrary ReqMsg where
    arbitrary = ReqMsg <$> arbitrary <*> arbitrary

instance (Bi Commitment) => Arbitrary DataMsg where
    arbitrary = oneof [ DMCommitment <$> arbitrary <*> arbitrary
                      , DMOpening <$> arbitrary <*> arbitrary
                      , DMShares <$> arbitrary <*> arbitrary
                      , DMVssCertificate <$> arbitrary <*> arbitrary
                      ]
