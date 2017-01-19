-- | Messages used in Update System networking.

module Pos.Update.Network.Types
       ( ProposalMsgTag (..)
       , VoteMsgTag (..)
       ) where

import           Data.Text.Buildable (Buildable (..))
import           Universum

import           Pos.Update.Core     (UpdateProposal, UpdateVote)
import           Pos.Util            (NamedMessagePart (..))

-- | Tag for proposal messages
data ProposalMsgTag = ProposalMsgTag deriving (Eq, Show)

instance NamedMessagePart ProposalMsgTag where
    nMessageName _ = "Update proposal tag"

instance Buildable ProposalMsgTag where
    build _ = "ProposalMsgTag"

-- | Instance for `UpdateProposal`
instance NamedMessagePart UpdateProposal where
    nMessageName _ = "Update proposal"

-- | Tag for vote messages
data VoteMsgTag = VoteMsgTag deriving (Eq, Show)

instance NamedMessagePart VoteMsgTag where
    nMessageName _ = "Update vote tag"

instance Buildable VoteMsgTag where
    build _ = "VoteMsgTag"

-- | Instance for `UpdateVote`
instance NamedMessagePart UpdateVote where
    nMessageName _ = "Update vote"
