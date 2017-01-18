-- | Types which are stored in memory.

module Pos.Update.MemState.Types
       ( MemPool (..)
       , UpdateProposals
       , GlobalVotes

       -- , LocalProposalState (..)
       -- , fixLocalState
       ) where

import           Data.Default    (Default (def))
import           Universum

import           Pos.Update.Core (ExtStakeholderVotes, UpId, UpdateProposal)

-- I suppose it's not needed nowadays.

-- -- | Local state of proposal
-- data LocalProposalState = LocalProposalState
--     { lpsVotes         :: !StakeholderVotes
--       -- ^ Votes given for this proposal.
--     , lpsProposal      :: !UpdateProposal
--       -- ^ Proposal itself.
--     , lpsPositiveStake :: !Coin
--       -- ^ Total stake of all positive votes.
--     , lpsNegativeStake :: !Coin
--       -- ^ Total stake of all negative votes.
--     }

-- -- | Function for saving local state of proposal into blockchain
-- fixLocalState :: LocalProposalState -> SlotId -> UndecidedProposalState
-- fixLocalState LocalProposalState{..} upsSlot = UndecidedProposalState
--     { upsVotes = lpsVotes
--     , upsProposal = lpsProposal
--     , upsPositiveStake = lpsPositiveStake
--     , upsNegativeStake = lpsNegativeStake
--     , ..
--     }

-- | MemPool is data maintained by node to be included into block and
-- relayed to other nodes.

type UpdateProposals = HashMap UpId UpdateProposal
type GlobalVotes = HashMap UpId ExtStakeholderVotes

data MemPool = MemPool
    { mpProposals   :: !UpdateProposals
    , mpGlobalVotes :: !GlobalVotes
    }

instance Default MemPool where
    def = MemPool mempty mempty
