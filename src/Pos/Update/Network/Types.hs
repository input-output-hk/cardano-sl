-- | Messages used in Update System networking.

module Pos.Update.Network.Types
       ( ProposalMsgTag (..)
       , VoteMsgTag (..)
       ) where

import           Data.Text.Buildable (Buildable (..))
import           Universum

import           Pos.Update.Core     (UpdateProposal)
import           Pos.Util            (NamedMessagePart (..))

-- | Tag for proposal messages
data ProposalMsgTag = ProposalMsgTag deriving (Eq, Show)

instance NamedMessagePart ProposalMsgTag where
    nMessageName _ = "Update proposal tag"

instance Buildable ProposalMsgTag where
    build _ = "ProposalMsgTag"

-- | Tag for vote messages
data VoteMsgTag = VoteMsgTag deriving (Eq, Show)

instance NamedMessagePart VoteMsgTag where
    nMessageName _ = "Update vote tag"

instance Buildable VoteMsgTag where
    build _ = "VoteMsgTag"

-- | Instance for `UpdateProposal`
instance NamedMessagePart UpdateProposal where
    nMessageName _ = "Update proposal"
