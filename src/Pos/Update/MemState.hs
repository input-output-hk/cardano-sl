-- | In-memory state of Update System.

module Pos.Update.MemState
       ( MemState (..)
       , LocalProposalState (..)
       , fixLocalState
       ) where

import           Data.Default        (Default (def))
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Universum

import           Pos.DB.Types        (UndecidedProposalState (..))
import           Pos.Types           (Coin, EpochIndex, SlotId)
import           Pos.Update.Types    (StakeholderVotes, UpId, UpdateProposal)

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

data MemState = MemState
    { msProposals   :: !(HashMap UpId LocalProposalState)
    , msEpoch       :: EpochIndex
    , msGlobalVotes :: !(HashMap UpId StakeholderVotes)
    }

instance Default MemState where
    def =
        MemState
        { msProposals = HM.empty
        , msEpoch = 0
        , msGlobalVotes = HM.empty
        }
