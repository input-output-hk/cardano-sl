module Pos.Update.Types.Communication
       ( ProposalMsgTag (..)
       , VoteMsgTag (..)
       ) where

import           Data.Text.Buildable    (Buildable (..))
import           Pos.Update.Types.Types (UpdateProposal)
import           Pos.Util               (NamedMessagePart (..))

-- | Tag for proposal messages
data ProposalMsgTag = ProposalMsgTag

instance NamedMessagePart ProposalMsgTag where
    nMessageName _ = "Update proposal tag"

instance Buildable ProposalMsgTag where
    build _ = "ProposalMsgTag"

-- | Tag for vote messages
data VoteMsgTag = VoteMsgTag

instance NamedMessagePart VoteMsgTag where
    nMessageName _ = "Update vote tag"

instance Buildable VoteMsgTag where
    build _ = "VoteMsgTag"

-- | Instance for `UpdateProposal`
instance NamedMessagePart UpdateProposal where
    nMessageName _ = "Update proposal"
