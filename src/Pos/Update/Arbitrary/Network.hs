{-# LANGUAGE TemplateHaskell #-}

-- | Arbitrary instances for Update System networking types.

module Pos.Update.Arbitrary.Network
       (
       ) where

import           Data.DeriveTH             (derive, makeArbitrary)
import           Test.QuickCheck           (Arbitrary (..), listOf, vector)
import           Universum

import           Pos.Binary.Update         ()
import           Pos.Communication.Limits  (MaxSize (..), updateVoteNumLimit)
import           Pos.Communication.Relay   (DataMsg (..))
import           Pos.Crypto                (hash, sign, toPublic)
import           Pos.Types.Arbitrary       ()
import           Pos.Update.Arbitrary.Core ()
import           Pos.Update.Core.Types     (UpdateProposal (..), UpdateVote (..))
import           Pos.Update.Network.Types  (ProposalMsgTag (..), VoteMsgTag (..))

derive makeArbitrary ''ProposalMsgTag
derive makeArbitrary ''VoteMsgTag

instance Arbitrary (DataMsg UpdateVote) where
    arbitrary = DataMsg <$> arbitrary

instance Arbitrary (DataMsg (UpdateProposal, [UpdateVote])) where
    arbitrary = do
        up <- arbitrary
        let id = hash up
            genVote = do
                sk <- arbitrary
                let pk = toPublic sk
                decision <- arbitrary
                pure $ UpdateVote pk id decision $ sign sk (id, decision)
        votes <- listOf genVote
        pure $ DataMsg (up, votes)

instance Arbitrary (MaxSize (DataMsg (UpdateProposal, [UpdateVote]))) where
    arbitrary =
        MaxSize . DataMsg <$> ((,) <$> arbitrary <*> vector updateVoteNumLimit)
