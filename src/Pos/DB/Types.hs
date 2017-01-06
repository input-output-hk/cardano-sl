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
       ) where

import           Control.Lens     (makeLenses)
import qualified Database.RocksDB as Rocks
import           Universum

import           Pos.Crypto       (PublicKey)
import           Pos.Types        (Block, Coin, EpochIndex, Richmen, SlotId, SlotLeaders,
                                   UpdateProposal)

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
    deriving (Generic)

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
