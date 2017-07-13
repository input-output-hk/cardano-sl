-- | Core types used in 'Slotting'.

module Pos.Slotting.Types
       ( EpochSlottingData (..)
       , SlottingData (..)
       ) where

import           Universum

import           Data.Time.Units (Millisecond)

import           Pos.Core        (EpochIndex, Timestamp)
import           Pos.Util.Util   ()

-- | Data which is necessary for slotting and corresponds to a particular epoch.
data EpochSlottingData = EpochSlottingData
    { esdSlotDuration :: !Millisecond
    -- ^ Slot duration actual for given epoch.
    , esdStartDiff    :: !Timestamp
    -- ^ Difference between epoch start and system start time
    } deriving (Eq, Show, Generic)

instance NFData EpochSlottingData

-- | Data necessary for slotting to work which is basically part of GState.
data SlottingData = SlottingData
    { sdPenult      :: !EpochSlottingData
    -- ^ 'EpochSlottingData' for penult (i. e. 'sdPenultEpoch') epoch.
    , sdLast        :: !EpochSlottingData
    -- ^ 'EpochSlottingData' for last epoch.
    -- Last means 'sdPenultEpoch + 1'.
    , sdPenultEpoch :: !EpochIndex
    -- ^ Penult epoch for which 'EpochSlottingData' is known.
    } deriving (Eq, Show, Generic)

instance NFData SlottingData
