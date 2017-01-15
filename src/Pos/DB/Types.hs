{-# LANGUAGE TemplateHaskell #-}

-- | Types related to DB.

module Pos.DB.Types
       (
         -- * General types.
         DB (..)
       , NodeDBs (..)
       , blockDB
       , gStateDB
       , lrcDB
       , miscDB

       -- * Snapshot
       , Snapshot (..)
       , usingSnapshot

        -- * Block DB related types.
       , StoredBlock (..)

        -- * LRC related types.
       , LeadersStorage (..)
       , GtRichmenStorage (..)

        -- * Update System related types.
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
import           Pos.Lrc.Types       (RichmenStake)
import           Pos.Types           (Block, ChainDifficulty, Coin, EpochIndex, SlotId,
                                      SlotLeaders, mkCoin, unsafeAddCoin)
import           Pos.Update.Types    (StakeholderVotes, UpdateProposal, combineVotes)

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
    { _blockDB  :: !(DB ssc) -- ^ Blocks, block index, undo data.
    , _gStateDB :: !(DB ssc) -- ^ Global state corresponding to some tip.
    , _lrcDB    :: !(DB ssc) -- ^ Data computed by LRC.
    , _miscDB   :: !(DB ssc) -- ^ Everything small and insignificant
    }

makeLenses ''NodeDBs

----------------------------------------------------------------------------
-- Snapshot
----------------------------------------------------------------------------
newtype Snapshot = Snapshot Rocks.Snapshot

usingSnapshot :: (MonadIO m, MonadMask m) => DB ssc -> (Snapshot -> m a) -> m a
usingSnapshot DB{..} action =
    bracket (Rocks.createSnapshot rocksDB) (Rocks.releaseSnapshot rocksDB)
            (action . Snapshot)

----------------------------------------------------------------------------
-- Blocks DB
----------------------------------------------------------------------------

-- Todo maybe remove this wrapper at all?
data StoredBlock ssc = StoredBlock
    { sbBlock  :: !(Block ssc)  -- ^ Block itself.
    } deriving (Generic)

----------------------------------------------------------------------------
-- LRC
----------------------------------------------------------------------------

data LeadersStorage ssc = LeadersStorage
    { lrcEpoch   :: !EpochIndex
    , lrcLeaders :: !SlotLeaders
    } deriving (Generic)

data GtRichmenStorage ssc = GtRichmenStorage
    { gtRichmenEpoch :: !EpochIndex
    , gtRichmen      :: !RichmenStake
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
      deriving (Generic)

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
