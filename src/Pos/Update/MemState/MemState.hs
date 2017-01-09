-- | In-memory state of Update System.

module Pos.Update.MemState.MemState
       ( MemState (..)
       ) where

import           Data.Default (Default (def))
-- import           Universum

import           Pos.Types    (EpochIndex)

data MemState = MemState
    { msEpoch :: !EpochIndex
    }

instance Default MemState where
    def =
        MemState
        { msEpoch = 0
        }
