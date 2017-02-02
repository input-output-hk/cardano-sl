-- | Core types used in 'Slotting'.

module Pos.Slotting.Types
       ( SlottingData (..)
       ) where

import           Data.Time.Units (Millisecond)
-- import           Universum

import           Pos.Types       (EpochIndex)

data SlottingData = SlottingData
    { sdEpoch        :: !EpochIndex
    , sdSlotDuration :: !Millisecond
    , sdEpochStart   :: !EpochIndex
    }
