-- | In-memory state of Update System.

module Pos.Update.MemState.MemState
       ( MemState (..)
       ) where

import           Data.Default              (Default (def))
-- import           Universum

import           Pos.Types                 (SlotId (..))
import           Pos.Update.MemState.Types (MemPool)
import           Pos.Update.Poll.Types     (PollModifier)

-- TODO: store tip here.
-- | MemState contains all in-memory data necesary for Update System.
data MemState = MemState
    { msSlot     :: !SlotId
    -- ^ Slot for which data is valid.
    -- In reality EpochIndex should be enough, but we sometimes
    -- overgeneralize things.
    , msPool     :: !MemPool
    -- ^ Pool of data to be included into block.
    , msModifier :: !PollModifier
    -- ^ Modifier of GState corresponding to 'msPool'.
    }

instance Default MemState where
    def =
        MemState
        { msSlot = SlotId 0 0
        , msPool = def
        , msModifier = def
        }
