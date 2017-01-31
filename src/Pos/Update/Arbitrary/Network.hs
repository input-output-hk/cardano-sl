{-# LANGUAGE TemplateHaskell #-}

-- | Arbitrary instances for Update System networking types.

module Pos.Update.Arbitrary.Network
       (
       ) where

import           Data.DeriveTH             (derive, makeArbitrary)
import           Test.QuickCheck           (Arbitrary (..))
import           Universum

import           Pos.Update.Arbitrary.Core ()
import           Pos.Update.Network.Types  (ProposalMsgTag (..), VoteMsgTag (..))

import           Pos.Binary.Update         ()
import           Pos.Types.Arbitrary       ()
import           Pos.Update.Core.Types     (UpdateVote (..), VoteId, mkVoteId)
import           Pos.Communication.Relay            (DataMsg (..))

derive makeArbitrary ''ProposalMsgTag
derive makeArbitrary ''VoteMsgTag

instance Arbitrary (DataMsg VoteId UpdateVote) where
    arbitrary = do
        vote <- arbitrary
        return $ DataMsg vote (mkVoteId vote)
