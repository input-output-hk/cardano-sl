-- | Messages used in Update System networking.

module Pos.Update.Network.Types
       ( ProposalMsgTag (..)
       , VoteMsgTag (..)
       ) where

import           Data.Text.Buildable (Buildable (..))
import           Universum

-- | Tag for proposal messages
data ProposalMsgTag = ProposalMsgTag deriving (Eq, Show)

instance Buildable ProposalMsgTag where
    build _ = "ProposalMsgTag"

-- | Tag for vote messages
data VoteMsgTag = VoteMsgTag deriving (Eq, Show)

instance Buildable VoteMsgTag where
    build _ = "VoteMsgTag"
