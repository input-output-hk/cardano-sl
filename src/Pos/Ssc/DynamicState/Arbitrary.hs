{-# LANGUAGE TemplateHaskell #-}

-- | Arbitrary instances for DynamicState types.

module Pos.Ssc.DynamicState.Arbitrary
       ( CommitmentOpening (..)
       ) where

import           Data.DeriveTH              (derive, makeArbitrary)
import           Data.List.NonEmpty         (NonEmpty ((:|)))
import           Test.QuickCheck            (Arbitrary (..), elements)
import           Universum

import           Pos.Crypto                 (SecretProof, SecretSharingExtra,
                                             deterministicVssKeyGen, toVssPublicKey)
import           Pos.Crypto.Arbitrary       ()
import           Pos.Ssc.DynamicState.Base  (Commitment (..), Opening,
                                             genCommitmentAndOpening)
import           Pos.Ssc.DynamicState.Types (DSProof (..))
import           Pos.Types.Arbitrary.Unsafe ()
import           Pos.Util.Arbitrary         (Nonrepeating (..), sublistN, unsafeMakePool)

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
    genCommitmentAndOpening 1 (vssPk :| [])
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

-- TODO: these types are not in Pos.Ssc.DynamicState actually, but they are
-- needed and it's not so easy to do it better
instance Arbitrary SecretSharingExtra where
    arbitrary = commExtra <$> arbitrary

instance Arbitrary SecretProof where
    arbitrary = commProof <$> arbitrary

derive makeArbitrary ''DSProof
