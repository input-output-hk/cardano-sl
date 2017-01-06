{-# LANGUAGE TemplateHaskell #-}

-- | Types related to DB.

module Pos.DB.Types
       (
         -- * General types.
         DB (..)
       , NodeDBs (..)
       , blockDB
       , utxoDB
       , miscDB

        -- * Block DB related types.
       , StoredBlock (..)

        -- * LRC related types.
       , LrcStorage (..)

        -- * Update System related types.
       , VoteState (..)
       , ProposalState (..)
       , canCombineVotes
       , combineVotes
       , mkProposalState
       , voteToProposalState
       ) where

import           Control.Exception   (assert)
import           Control.Lens        (makeLenses)
import qualified Data.HashMap.Strict as HM
import qualified Database.RocksDB    as Rocks
import           Formatting          (sformat, shown, (%))
import           Universum

import           Pos.Crypto          (PublicKey)
import           Pos.Types           (Block, Coin, EpochIndex, Richmen, SlotId,
                                      SlotLeaders, UpdateProposal, mkCoin, unsafeAddCoin)

----------------------------------------------------------------------------
-- General
----------------------------------------------------------------------------

-- should we replace `rocks` prefix by other or remove it at all?
data DB ssc = DB
    { rocksReadOpts  :: !Rocks.ReadOptions
    , rocksWriteOpts :: !Rocks.WriteOptions
    , rocksOptions   :: !Rocks.Options
    , rocksDB        :: !Rocks.DB
    }

data NodeDBs ssc = NodeDBs
    { _blockDB :: DB ssc -- ^ Blocks, block index, undo data.
    , _utxoDB  :: DB ssc -- ^ Txs-related data.
    , _miscDB  :: DB ssc -- ^ Everything small and insignificant
{-    , _updateDB :: DB ssc -- ^ Update-related data -}
    }

makeLenses ''NodeDBs

----------------------------------------------------------------------------
-- Blocks DB
----------------------------------------------------------------------------

data StoredBlock ssc = StoredBlock
    { sbBlock  :: !(Block ssc)  -- ^ Block itself.
    , sbInMain :: !Bool         -- ^ Whether block is part of our main chain.
    } deriving (Generic)

----------------------------------------------------------------------------
-- LRC
----------------------------------------------------------------------------

data LrcStorage ssc = LrcStorage
    { lrcEpoch   :: !EpochIndex
    , lrcLeaders :: !SlotLeaders
    , lrcRichmen :: !Richmen
    } deriving (Generic)

----------------------------------------------------------------------------
-- Update System
----------------------------------------------------------------------------

-- | This type represents summary of votes issued by stakeholder.
data VoteState
    = PositiveVote    -- ^ Stakeholder voted once positively.
    | NegativeVote    -- ^ Stakeholder voted once positively.
    | PositiveRevote  -- ^ Stakeholder voted negatively, then positively.
    | NegativeRevote  -- ^ Stakeholder voted positively, then negatively.
    deriving (Show, Generic)

-- | Check whether given decision is a valid vote if applied to
-- existing vote (which may not exist).
canCombineVotes :: Bool -> Maybe VoteState -> Bool
canCombineVotes _ Nothing                 = True
canCombineVotes True (Just NegativeVote)  = True
canCombineVotes False (Just PositiveVote) = True
canCombineVotes _ _                       = False

-- | Apply decision to given vote (or Nothing). This function will
-- 'panic' if decision can't be applied. Use 'canCombineVotes' in
-- advance.
combineVotes :: Bool -> Maybe VoteState -> VoteState
combineVotes decision oldVote = assert (canCombineVotes decision oldVote) combineVotesDo
  where
    combineVotesDo =
        case (decision, oldVote) of
            (True, Nothing)            -> PositiveVote
            (False, Nothing)           -> NegativeVote
            (True, Just NegativeVote)  -> PositiveRevote
            (False, Just PositiveVote) -> NegativeRevote
            (_, Just vote)             -> onFailure vote
    onFailure =
        panic .
        sformat
        ("combineVotes: these votes can't be combined ("%shown%" and "%shown%")")
        decision

-- | State of UpdateProposal.
data ProposalState = ProposalState
    { psVotes         :: !(HashMap PublicKey VoteState)
      -- ^ Votes given for this proposal.
    , psProposal      :: !UpdateProposal
      -- ^ Proposal itself.
    , psSlot          :: !SlotId
      -- ^ SlotId from block in which update was proposed.
    , psPositiveStake :: !Coin
      -- ^ Total stake of all positive votes.
    , psNegativeStake :: !Coin
      -- ^ Total stake of all negative votes.
    } deriving (Generic)

-- | Make ProposalState from immutable data, i. e. SlotId and UpdateProposal.
mkProposalState :: SlotId -> UpdateProposal -> ProposalState
mkProposalState psSlot psProposal =
    ProposalState
    { psVotes = mempty
    , psPositiveStake = mkCoin 0
    , psNegativeStake = mkCoin 0
    , ..
    }

-- | Apply vote to ProposalState, thus modifing mutable data,
-- i. e. votes and stakes.
voteToProposalState :: PublicKey -> Coin -> Bool -> ProposalState -> ProposalState
voteToProposalState voter stake decision ProposalState {..} =
    ProposalState
    { psVotes = HM.alter (Just . combineVotes decision) voter psVotes
    , psPositiveStake = newPositiveStake
    , psNegativeStake = newNegativeStake
    , ..
    }
  where
    newPositiveStake
        | decision = psPositiveStake `unsafeAddCoin` stake
        | otherwise = psPositiveStake
    newNegativeStake
        | decision = psNegativeStake
        | otherwise = psNegativeStake `unsafeAddCoin` stake
