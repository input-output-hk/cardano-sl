-- | Types which are stored in memory.

module Pos.Update.MemState.Types
       ( LocalProposalState (..)
       , fixLocalState

       , MemPool (..)
       , PollModifier (..)
       ) where

-- import           Data.Default     (Default (def))
import           Universum

import           Pos.DB.Types    (ProposalState, UndecidedProposalState (..))
import           Pos.Script.Type (ScriptVersion)
import           Pos.Types       (ApplicationName, Coin, NumSoftwareVersion,
                                  ProtocolVersion, SlotId)
import           Pos.Update.Core (StakeholderVotes, UpId, UpdateProposal)

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

-- | PollModifier is used in verification. It represents operation which
-- one should apply to global state to obtain result of application of
-- MemPool or blocks which are verified.
data PollModifier = PollModifier
    { pmNewScriptVersions :: !(HashMap ProtocolVersion ScriptVersion)
    , pmLastAdoptedPV     :: !ProtocolVersion
    , pmNewConfirmed      :: !(HashMap ApplicationName NumSoftwareVersion)
    , pmNewActiveProps    :: !(HashMap UpId ProposalState)
    , pmDelActiveProps    :: !(HashSet UpId)
    , pmNewActivePropsIdx :: !(HashMap ApplicationName UpId)
    , pmDelActivePropsIdx :: !(HashSet ApplicationName)
    }
