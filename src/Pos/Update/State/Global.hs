{-# LANGUAGE TemplateHaskell #-}

-- | Global state of Update System. Contains relevant summary of data
-- stored in blocks. This state is stored in memory only. Another part
-- is stored in GStateDB, see Pos.DB.GState.

module Pos.Update.State.Global
       ( GlobalState (..)
       , HasGlobalState (globalState)
       ) where

import           Control.Lens (makeClassy, makeLenses)
import           Data.Default (Default (def))
import           Universum

import           Pos.Crypto   (PublicKey)
import           Pos.Types    (SlotId, SoftwareVersion, UpdateProposal)

-- | This type represents summary of votes issued by stakeholder.
data VoteState
    = PositiveVote    -- ^ Stakeholder voted once positively.
    | NegativeVote    -- ^ Stakeholder voted once positively.
    | PositiveRevote  -- ^ Stakeholder voted negatively, then positively.
    | NegativeRevote  -- ^ Stakeholder voted positively, then negatively.

-- | State of UpdateProposal.
data ProposalState = ProposalState
    { _psVotes    :: !(HashMap PublicKey VoteState)
      -- ^ Votes given for this proposal.
    , _psProposal :: !UpdateProposal
      -- ^ Proposal itself.
    , _psSlot     :: !SlotId
      -- ^ SlotId from block in which update was proposed.
    }

makeLenses ''ProposalState

-- | In-memory global state of Update System.
data GlobalState = GlobalState
    { _gsProposals :: !(HashMap SoftwareVersion ProposalState)
    }

-- | Classy lenses generated for GlobalState.
makeClassy ''GlobalState

instance Default GlobalState where
    def =
        GlobalState
        { _gsProposals = mempty
        }
