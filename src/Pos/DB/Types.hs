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
       , UndecidedProposalState (..)
       , DecidedProposalState (..)
       , ProposalState (..)
       , psProposal
       , mkUProposalState
       , voteToUProposalState
       ) where

import           Control.Lens        (makeLenses)
import qualified Data.HashMap.Strict as HM
import qualified Database.RocksDB    as Rocks
import           Universum

import           Pos.Crypto          (PublicKey)
import           Pos.Types           (Block, ChainDifficulty, Coin, EpochIndex, Richmen,
                                      SlotId, SlotLeaders, StakeholderVotes,
                                      UpdateProposal, VoteState, combineVotes, mkCoin,
                                      unsafeAddCoin)

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

-- | State of UpdateProposal which can't be classified as approved or
-- rejected.
data UndecidedProposalState = UndecidedProposalState
    { upsVotes         :: !StakeholderVotes
      -- ^ Votes given for this proposal.
    , upsProposal      :: !UpdateProposal
      -- ^ Proposal itself.
    , upsSlot          :: !SlotId
      -- ^ SlotId from block in which update was proposed.
    , upsPositiveStake :: !Coin
      -- ^ Total stake of all positive votes.
    , upsNegativeStake :: !Coin
      -- ^ Total stake of all negative votes.
    } deriving (Generic)

-- | State of UpdateProposal which can be classified as approved or
-- rejected.
data DecidedProposalState = DecidedProposalState
    { dpsDecision   :: !Bool
      -- ^ Whether proposal is approved.
    , dpsProposal   :: !UpdateProposal
      -- ^ Proposal itself.
    , dpsDifficulty :: !ChainDifficulty
      -- ^ Difficulty at which this proposal became approved/rejected.
    } deriving (Generic)

-- | State of UpdateProposal.
data ProposalState
    = PSUndecided !UndecidedProposalState
    | PSDecided !DecidedProposalState

psProposal :: ProposalState -> UpdateProposal
psProposal (PSUndecided ups) = upsProposal ups
psProposal (PSDecided dps)   = dpsProposal dps

-- | Make UndecidedProposalState from immutable data, i. e. SlotId and
-- UpdateProposal.
mkUProposalState :: SlotId -> UpdateProposal -> UndecidedProposalState
mkUProposalState upsSlot upsProposal =
    UndecidedProposalState
    { upsVotes = mempty , upsPositiveStake = mkCoin 0
    , upsNegativeStake = mkCoin 0
    , ..
    }

-- | Apply vote to UndecidedProposalState, thus modifing mutable data,
-- i. e. votes and stakes.
voteToUProposalState ::
    PublicKey -> Coin -> Bool -> UndecidedProposalState -> UndecidedProposalState
voteToUProposalState voter stake decision UndecidedProposalState {..} =
    UndecidedProposalState
    { upsVotes = HM.alter (Just . combineVotes decision) voter upsVotes
    , upsPositiveStake = newPositiveStake
    , upsNegativeStake = newNegativeStake
    , ..
    }
  where
    newPositiveStake
        | decision = upsPositiveStake `unsafeAddCoin` stake
        | otherwise = upsPositiveStake
    newNegativeStake
        | decision = upsNegativeStake
        | otherwise = upsNegativeStake `unsafeAddCoin` stake
