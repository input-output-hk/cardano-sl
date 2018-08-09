{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pos.Core.Update.VoteId
       ( VoteId
       , mkVoteId
       ) where

import           Universum

import qualified Data.Text.Buildable as Buildable
import           Formatting (bprint, build, (%))

import           Pos.Crypto (PublicKey)

import           Pos.Core.Update.Proposal (UpId)
import           Pos.Core.Update.Vote (UpdateVote (..))

type VoteId = (UpId, PublicKey, Bool)

instance Buildable VoteId where
    build (upId, pk, dec) =
      bprint ("Vote Id { voter: "%build%", proposal id: "%build%", voter's decision: "%build%" }")
             pk upId dec

mkVoteId :: UpdateVote -> VoteId
mkVoteId vote = (uvProposalId vote, uvKey vote, uvDecision vote)
