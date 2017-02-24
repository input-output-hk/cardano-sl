-- | Core types used in 'Slotting'.

module Pos.Slotting.Types
       ( EpochSlottingData (..)
       , SlottingData (..)
       ) where

import           Data.Time.Units     (Millisecond)
import           Universum

import           Pos.Core.Types      (EpochIndex, Timestamp)
import           Pos.Core.Timestamp ()

-- | Data which is necessary for slotting and corresponds to a particular epoch.
data EpochSlottingData = EpochSlottingData
    { esdSlotDuration :: !Millisecond
    -- ^ Slot duration actual for given epoch.
    , esdStart        :: !Timestamp
    -- ^ Time when epoch starts.
    } deriving (Show)

-- | Data necessary for slotting to work which is basically part of GState.
data SlottingData = SlottingData
    { sdPenult      :: !EpochSlottingData
    -- ^ 'EpochSlottingData' for penult (i. e. 'sdPenultEpoch') epoch.
    , sdLast        :: !EpochSlottingData
    -- ^ 'EpochSlottingData' for last epoch.
    -- Last means 'sdPenultEpoch + 1'.
    , sdPenultEpoch :: !EpochIndex
    -- ^ Penult epoch for which 'EpochSlottingData' is known.
    } deriving (Show)
