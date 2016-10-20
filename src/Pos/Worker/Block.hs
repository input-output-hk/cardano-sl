-- | Block processing related workers.

module Pos.Worker.Block
       ( blkOnNewSlot
       , blkWorkers
       ) where

import           Control.Lens (ix, (^?))
import           Universum

import           Pos.State    (getLeaders)
import           Pos.Types    (SlotId (..))
import           Pos.WorkMode (WorkMode)

-- | Action which should be done when new slot starts.
blkOnNewSlot :: WorkMode m => SlotId -> m ()
blkOnNewSlot SlotId {..} = do
    leaders <- getLeaders siEpoch
    let _ = leaders ^? ix (fromIntegral siSlot)
    return ()  -- TODO

-- | All workers specific to block processing.
-- Exceptions:
-- 1. Worker which ticks when new slot starts.
blkWorkers :: WorkMode m => [m ()]
blkWorkers = []
