-- | In-memory state of Update System.

module Pos.Update.MemState.MemState
       ( MemState (..)
       ) where

import           Data.Default              (Default (def))
-- import           Universum

import           Pos.Types                 (EpochIndex)
import           Pos.Update.MemState.Types (MemPool)
import           Pos.Update.Poll.Types     (PollModifier)

-- TODO: store tip here.
-- | MemState contains all in-memory data necesary for Update System.
data MemState = MemState
    { msEpoch    :: !EpochIndex
    -- ^ Epoch for which data is valid.
    , msPool     :: !MemPool
    -- ^ Pool of data to be included into block.
    , msModifier :: !PollModifier
    -- ^ Modifier of GState corresponding to 'msPool'.
    }

instance Default MemState where
    def =
        MemState
        { msEpoch = 0
        , msPool = def
        , msModifier = def
        }
