-- | Block processing related workers.

module Pos.Worker.Block
       ( blkOnNewSlot
       , blkWorkers
       ) where

import           Control.Lens             (ix, (^?))
import           Control.TimeWarp.Logging (logInfo)
import           Control.TimeWarp.Timed   (for, wait)
import           Formatting               (sformat, (%))
import           Universum

import           Pos.Constants            (networkDiameter, slotDuration)
import           Pos.State                (getLeaders)
import           Pos.Types                (SlotId (..), slotIdF)
import           Pos.WorkMode             (WorkMode, getNodeContext, ncPublicKey)

-- | Action which should be done when new slot starts.
blkOnNewSlot :: WorkMode m => SlotId -> m ()
blkOnNewSlot slotId@SlotId {..} = do
    leaders <- getLeaders siEpoch
    ourPk <- ncPublicKey <$> getNodeContext
    let leader = leaders ^? ix (fromIntegral siSlot)
    when (leader == Just ourPk) $ onNewSlotWhenLeader slotId

onNewSlotWhenLeader :: WorkMode m => SlotId -> m ()
onNewSlotWhenLeader slotId = do
    logInfo $
        sformat ("I am leader of "%slotIdF%", I will create block soon") slotId
    wait $ for (slotDuration - networkDiameter)
    logInfo "It's time to create a block for current slot"
    -- TODO

-- | All workers specific to block processing.
-- Exceptions:
-- 1. Worker which ticks when new slot starts.
blkWorkers :: WorkMode m => [m ()]
blkWorkers = []
