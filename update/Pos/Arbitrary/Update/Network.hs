-- | Arbitrary instances for Update System networking types.

module Pos.Arbitrary.Update.Network
       (
       ) where

import           Universum

import           Test.QuickCheck (Arbitrary (..), listOf)

import           Pos.Arbitrary.Core ()
import           Pos.Arbitrary.Update.Core ()
import           Pos.Binary.Update ()
import           Pos.Communication.Relay (DataMsg (..))
import           Pos.Core.Update (UpdateProposal (..), UpdateVote (..), mkUpdateVote)
import           Pos.Crypto (ProtocolMagic, hash)

instance Arbitrary ProtocolMagic => Arbitrary (DataMsg UpdateVote) where
    arbitrary = DataMsg <$> arbitrary

instance Arbitrary ProtocolMagic => Arbitrary (DataMsg (UpdateProposal, [UpdateVote])) where
    arbitrary = do
        up <- arbitrary
        let id = hash up
            genVote = mkUpdateVote <$> arbitrary <*> arbitrary <*> pure id <*> arbitrary
        votes <- listOf genVote
        pure $ DataMsg (up, votes)

-- TODO [CSL-859]
-- FYI: difficulty now is that 'updateVoteNumLimit' is not constant.
-- instance Arbitrary (MaxSize (DataMsg (UpdateProposal, [UpdateVote]))) where
--     arbitrary =
--         -- we don't care about sensibility
--         MaxSize . DataMsg <$> ((,) <$> arbitrary <*> vector updateVoteNumLimit)
