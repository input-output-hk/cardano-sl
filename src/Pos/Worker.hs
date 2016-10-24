-- | High level workers.

module Pos.Worker
       ( runWorkers
       ) where

import           Control.TimeWarp.Logging (logInfo)
import           Control.TimeWarp.Timed   (fork_)
import           Formatting               (sformat, (%))
import           Universum

import           Pos.Slotting             (onNewSlot)
import           Pos.Types                (SlotId, slotIdF)
import           Pos.Worker.Block         (blkOnNewSlot, blkWorkers)
import           Pos.Worker.Mpc           (mpcOnNewSlot, mpcWorkers)
import           Pos.WorkMode             (WorkMode)

-- | Run all necessary workers in separate threads. This call doesn't
-- block.
runWorkers :: WorkMode m => m ()
runWorkers = mapM_ fork_ (onNewSlotWorker : blkWorkers ++ mpcWorkers)

onNewSlotWorker :: WorkMode m => m ()
onNewSlotWorker = onNewSlot False onNewSlotWorkerImpl

onNewSlotWorkerImpl :: WorkMode m => SlotId -> m ()
onNewSlotWorkerImpl slotId = do
    logInfo $ sformat ("New slot has just started: "%slotIdF) slotId
    -- TODO: what should be the order here?
    -- TODO: someone should actually generate the new genesis block. Maybe we
    -- could have 'mpcOnNewSlot' generate it and then have a function here
    -- that would bring it from MpcStorage to BlockStorage?
    mpcOnNewSlot slotId
    blkOnNewSlot slotId
