-- | High level workers.

module Pos.Worker
       ( runWorkers
       ) where

import           Control.TimeWarp.Logging (logInfo)
import           Control.TimeWarp.Timed   (fork_)
import           Formatting               (sformat, (%))
import           Universum

import           Pos.Slotting             (onNewSlot)
import           Pos.State                (processNewSlot)
import           Pos.Types                (SlotId, slotIdF)
import           Pos.Worker.Block         (blkOnNewSlot, blkWorkers)
import           Pos.Worker.Mpc           (mpcOnNewSlot, mpcWorkers)
import           Pos.Worker.Tx            (txWorkers)
import           Pos.WorkMode             (WorkMode)

-- | Run all necessary workers in separate threads. This call doesn't
-- block.
runWorkers :: WorkMode m => m ()
runWorkers = mapM_ fork_ (onNewSlotWorker : blkWorkers ++ mpcWorkers ++ txWorkers)

onNewSlotWorker :: WorkMode m => m ()
onNewSlotWorker = onNewSlot True onNewSlotWorkerImpl

onNewSlotWorkerImpl :: WorkMode m => SlotId -> m ()
onNewSlotWorkerImpl slotId = do
    logInfo $ sformat ("New slot has just started: "%slotIdF) slotId
    -- TODO: what should be the order here?
    processNewSlot slotId
    mpcOnNewSlot slotId
    blkOnNewSlot slotId
