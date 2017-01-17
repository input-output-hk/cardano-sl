-- | Types which are stored in memory.

module Pos.Update.MemState.Types
       ( LocalProposalState (..)
       , fixLocalState

       , MemPool (..)
       ) where

import           Data.Default          (Default (def))
import           Universum

import           Pos.Types             (Coin, SlotId)
import           Pos.Update.Core       (StakeholderVotes, UpId, UpdateProposal)
import           Pos.Update.Poll.Types (UndecidedProposalState (..))

-- | Local state of proposal
data LocalProposalState = LocalProposalState
    { lpsVotes         :: !StakeholderVotes
      -- ^ Votes given for this proposal.
    , lpsProposal      :: !UpdateProposal
      -- ^ Proposal itself.
    , lpsPositiveStake :: !Coin
      -- ^ Total stake of all positive votes.
    , lpsNegativeStake :: !Coin
      -- ^ Total stake of all negative votes.
    }

-- | Function for saving local state of proposal into blockchain
fixLocalState :: LocalProposalState -> SlotId -> UndecidedProposalState
fixLocalState LocalProposalState{..} upsSlot = UndecidedProposalState
    { upsVotes = lpsVotes
    , upsProposal = lpsProposal
    , upsPositiveStake = lpsPositiveStake
    , upsNegativeStake = lpsNegativeStake
    , ..
    }

-- | MemPool is data maintained by node to be included into block and
-- relayed to other nodes.
data MemPool = MemPool
    { mpProposals   :: !(HashMap UpId LocalProposalState)
    , mpGlobalVotes :: !(HashMap UpId StakeholderVotes)
    }

instance Default MemPool where
    def = MemPool mempty mempty
