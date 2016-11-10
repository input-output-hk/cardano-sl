{-# LANGUAGE TypeApplications #-}

-- | High level workers.

module Pos.Worker
       ( runWorkers
       , statsWorkers
       ) where

import           Control.TimeWarp.Logging (logDebug, logInfo, logNotice)
import           Control.TimeWarp.Timed   (fork_)
import           Data.Tagged              (untag)
import           Formatting               (build, sformat, (%))
import           Universum

import           Pos.Slotting             (onNewSlot)
import           Pos.Ssc.Class.Workers    (sscOnNewSlot, sscWorkers)
import           Pos.Ssc.DynamicState     (SscDynamicState)
import           Pos.State                (processNewSlot)
import           Pos.Types                (SlotId, slotIdF)
import           Pos.Util                 (logWarningWaitLinear)
import           Pos.Util.JsonLog         (jlCreatedBlock, jlLog)
import           Pos.Worker.Block         (blkOnNewSlot, blkWorkers)
import           Pos.Worker.Mpc           ()
import           Pos.Worker.Stats         (statsWorkers)
import           Pos.Worker.Tx            (txWorkers)
import           Pos.WorkMode             (WorkMode)

-- | Run all necessary workers in separate threads. This call doesn't
-- block.
runWorkers :: WorkMode m => m ()
runWorkers = mapM_ fork_ $ concat
    [ [onNewSlotWorker]
    , blkWorkers
    , untag @SscDynamicState sscWorkers
    , txWorkers
    ]

onNewSlotWorker :: WorkMode m => m ()
onNewSlotWorker = onNewSlot True onNewSlotWorkerImpl

onNewSlotWorkerImpl :: WorkMode m => SlotId -> m ()
onNewSlotWorkerImpl slotId = do
    logNotice $ sformat ("New slot has just started: "%slotIdF) slotId
    -- A note about order: currently only one thing is important, that
    -- `processNewSlot` is executed before everything else
    mGenBlock <- processNewSlot slotId
    forM_ mGenBlock $ \createdBlk -> do
      logInfo $ sformat ("Created genesis block:\n" %build) createdBlk
      jlLog $ jlCreatedBlock (Left createdBlk)
    logDebug "Finished `processNewSlot`"

    fork_ $ do
        logWarningWaitLinear 8 "mpcOnNewSlot" $
            untag @SscDynamicState sscOnNewSlot slotId
        logDebug "Finished `mpcOnNewSlot`"
    blkOnNewSlot slotId
    logDebug "Finished `blkOnNewSlot`"
