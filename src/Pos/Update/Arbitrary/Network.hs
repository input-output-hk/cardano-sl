{-# LANGUAGE TemplateHaskell #-}

-- | Arbitrary instances for Update System networking types.

module Pos.Update.Arbitrary.Network
       (
       ) where

import           Data.DeriveTH             (derive, makeArbitrary)
import           Test.QuickCheck           (Arbitrary (..), listOf)
import           Universum

import           Pos.Binary.Update         ()
import           Pos.Crypto                (hash, sign, toPublic)
import           Pos.Types.Arbitrary       ()
import           Pos.Update.Arbitrary.Core ()
import           Pos.Update.Core.Types     (UpdateProposal (..), UpdateVote (..))
import           Pos.Update.Network.Types  (ProposalMsgTag (..), VoteMsgTag (..))
import           Pos.Util.Relay            (DataMsg (..))

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
