{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Arbitrary instances for Update System networking types.

module Test.Pos.Infra.Arbitrary.Update
       (
       ) where

import           Universum hiding (id)

import           Test.QuickCheck (Arbitrary (..), listOf)

import           Pos.Chain.Update (UpdateProposal (..), UpdateVote (..),
                     mkUpdateVote)
import           Pos.Crypto (hash)
import           Pos.Infra.Communication.Relay (DataMsg (..))

import           Test.Pos.Chain.Update.Arbitrary ()
import           Test.Pos.Core.Arbitrary ()
import           Test.Pos.Crypto.Dummy (dummyProtocolMagic)

instance Arbitrary (DataMsg UpdateVote) where
    arbitrary = DataMsg <$> arbitrary

instance Arbitrary (DataMsg (UpdateProposal, [UpdateVote])) where
    arbitrary = do
        up <- arbitrary
        let id = hash up
            genVote = mkUpdateVote dummyProtocolMagic <$> arbitrary <*> pure id <*> arbitrary
        votes <- listOf genVote
        pure $ DataMsg (up, votes)

-- TODO [CSL-859]
-- FYI: difficulty now is that 'updateVoteNumLimit' is not constant.
-- instance Arbitrary (MaxSize (DataMsg (UpdateProposal, [UpdateVote]))) where
--     arbitrary =
--         -- we don't care about sensibility
--         MaxSize . DataMsg <$> ((,) <$> arbitrary <*> vector updateVoteNumLimit)
