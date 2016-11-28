{-# LANGUAGE TemplateHaskell #-}

-- | Arbitrary instances for GodTossing types.

module Pos.Ssc.GodTossing.Arbitrary
       ( CommitmentOpening (..)
       ) where

import           Data.DeriveTH                  (derive, makeArbitrary)
import           Data.List.NonEmpty             (NonEmpty ((:|)))
import           Test.QuickCheck                (Arbitrary (..), choose, elements)
import           Universum

import           Pos.Crypto                     (LSecretProof, LSecretSharingExtra,
                                                 deterministicVssKeyGen, toVssPublicKey)
import           Pos.Crypto.Arbitrary           ()
import           Pos.Ssc.GodTossing.Functions   (genCommitmentAndOpening)
import           Pos.Ssc.GodTossing.Types.Base  (Commitment (..), Opening)
import           Pos.Ssc.GodTossing.Types.Types (GtProof (..))
import           Pos.Types.Arbitrary.Unsafe     ()
import           Pos.Util                       (serialize)
import           Pos.Util.Arbitrary             (Nonrepeating (..), sublistN,
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
       genCommitmentAndOpening 1 (serialize vssPk :| [])
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

instance Arbitrary LSecretSharingExtra where
    arbitrary = commExtra <$> arbitrary

instance Arbitrary LSecretProof where
    arbitrary = commProof <$> arbitrary

derive makeArbitrary ''GtProof
