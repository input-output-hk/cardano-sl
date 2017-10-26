{-# LANGUAGE DeriveLift #-}

-- | This module contains all basic types for @cardano-sl@ update system.

module Pos.Update.Core.Types
       (
         StakeholderVotes
       , LocalVotes

       -- * VoteState
       , VoteState (..)
       , canCombineVotes
       , combineVotes
       , isPositiveVote
       , newVoteState
       ) where

import           Universum

import qualified Data.Text.Buildable as Buildable
import           Instances.TH.Lift   ()

import           Pos.Binary.Crypto   ()
import           Pos.Core.Update     (UpId, UpdateVote)
import           Pos.Crypto          (PublicKey)

----------------------------------------------------------------------------
-- VoteState
----------------------------------------------------------------------------

-- | This type represents summary of votes issued by stakeholder.
data VoteState
    = PositiveVote    -- ^ Stakeholder voted once positively.
    | NegativeVote    -- ^ Stakeholder voted once positively.
    | PositiveRevote  -- ^ Stakeholder voted negatively, then positively.
    | NegativeRevote  -- ^ Stakeholder voted positively, then negatively.
    deriving (Show, Generic, Eq)

instance NFData VoteState

instance Buildable VoteState where
    build PositiveVote   = "PositiveVote"
    build NegativeVote   = "NegativeVote"
    build PositiveRevote = "PositiveRevote"
    build NegativeRevote = "NegativeRevote"

-- | Create new VoteState from bool, which is simple vote, not revote.
newVoteState :: Bool -> VoteState
newVoteState True  = PositiveVote
newVoteState False = NegativeVote

isPositiveVote :: VoteState -> Bool
isPositiveVote PositiveVote   = True
isPositiveVote PositiveRevote = True
isPositiveVote _              = False

-- | Check whether given decision is a valid vote if applied to
-- existing vote (which may not exist).
canCombineVotes :: Bool -> Maybe VoteState -> Bool
canCombineVotes _ Nothing                 = True
canCombineVotes True (Just NegativeVote)  = True
canCombineVotes False (Just PositiveVote) = True
canCombineVotes _ _                       = False

-- | Apply decision to given vote (or Nothing). This function returns
-- 'Nothing' if decision can't be applied. 'canCombineVotes' can be
-- used to check whether it will be successful.
combineVotes :: Bool -> Maybe VoteState -> Maybe VoteState
combineVotes decision oldVote =
    case (decision, oldVote) of
        (True, Nothing)            -> Just PositiveVote
        (False, Nothing)           -> Just NegativeVote
        (True, Just NegativeVote)  -> Just PositiveRevote
        (False, Just PositiveVote) -> Just NegativeRevote
        (_, Just _)                -> Nothing

-- | Type alias for set of votes from stakeholders
type StakeholderVotes = HashMap PublicKey VoteState
type LocalVotes = HashMap UpId (HashMap PublicKey UpdateVote)
