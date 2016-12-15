{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

-- | Types related to DB.

module Pos.Modern.DB.Types
       (
         -- * General types.
         DB (..)
       , NodeDBs (..)
       , blockDB
       , utxoDB

        -- * Block DB related types.
       , StoredBlock (..)
       ) where

import           Control.Lens     (makeLenses)
import qualified Database.RocksDB as Rocks
import           Universum

import           Pos.Types        (Block)

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
    }

makeLenses ''NodeDBs

----------------------------------------------------------------------------
-- Blocks DB
----------------------------------------------------------------------------

data StoredBlock ssc = StoredBlock
    { sbBlock  :: !(Block ssc)  -- ^ Block itself.
    , sbInMain :: !Bool         -- ^ Whether block is part of our main chain.
    } deriving (Generic)
