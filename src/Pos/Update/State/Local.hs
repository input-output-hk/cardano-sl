{-# LANGUAGE TemplateHaskell #-}

-- | Local State of Update System. Contains data which is not stored
-- in blocks, but can appear there.

module Pos.Update.State.Local
       ( LocalState (..)
       , HasLocalState (localState)
       ) where

import           Control.Lens (makeClassy)
import           Data.Default (Default (def))
import           Universum

import           Pos.Types    (UpdateVote)

data LocalState = LocalState
    { _lsVotes :: ![UpdateVote]
    }

-- | Classy lenses generated for LocalState.
makeClassy ''LocalState

instance Default LocalState where
    def =
        LocalState
        { _lsVotes = mempty
        }
