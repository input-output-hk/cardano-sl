-- | Block processing related workers.

module Pos.Worker.Block
       ( blkOnNewSlot
       , blkWorkers
       ) where

import           Universum

import           Pos.Types    (SlotId)
import           Pos.WorkMode (WorkMode)

-- | Action which should be done when new slot starts.
blkOnNewSlot :: WorkMode m => SlotId -> m ()
blkOnNewSlot = notImplemented  -- generate a block if we are leader

-- | All workers specific to block processing.
-- Exceptions:
-- 1. Worker which ticks when new slot starts.
blkWorkers :: WorkMode m => [m ()]
blkWorkers = []
