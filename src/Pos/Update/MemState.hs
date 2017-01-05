-- | In-memory state of Update System.

module Pos.Update.MemState
       ( MemState (..)
       ) where

import           Data.Default (Default (def))
import           Universum

import           Pos.Types    (UpdateVote)

data MemState = MemState
    { msVotes :: ![UpdateVote]
    }

instance Default MemState where
    def =
        MemState
        { msVotes = mempty
        }
