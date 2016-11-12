{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE Rank2Types             #-}
{-# LANGUAGE ScopedTypeVariables    #-}

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
import           Pos.Ssc.Class.Workers    (SscWorkersClass, sscOnNewSlot, sscWorkers)
import           Pos.State                (processNewSlot)
import           Pos.Types                (SlotId, slotIdF)
import           Pos.Util                 (logWarningWaitLinear)
import           Pos.Util.JsonLog         (jlCreatedBlock, jlLog)
import           Pos.Worker.Block         (blkOnNewSlot, blkWorkers)
import           Pos.Worker.Stats         (statsWorkers)
import           Pos.Worker.Tx            (txWorkers)
import           Pos.WorkMode             (WorkMode)

-- | Run all necessary workers in separate threads. This call doesn't
-- block.
runWorkers :: forall ssc m . (SscWorkersClass ssc,  WorkMode ssc m) => m ()
runWorkers = mapM_ fork_ $ concat
    [ [onNewSlotWorker @ssc]
    , blkWorkers @ssc
    , untag @ssc sscWorkers
    , txWorkers @ssc
    ]

onNewSlotWorker :: forall ssc m . (SscWorkersClass ssc, WorkMode ssc m) => m ()
onNewSlotWorker = onNewSlot True (onNewSlotWorkerImpl @ssc)

onNewSlotWorkerImpl :: forall ssc m . (SscWorkersClass ssc, WorkMode ssc m)
                    => SlotId -> m ()
onNewSlotWorkerImpl slotId = do
    logNotice $ sformat ("New slot has just started: "%slotIdF) slotId
    -- A note about order: currently only one thing is important, that
    -- `processNewSlot` is executed before everything else
    mGenBlock <- processNewSlot @ssc slotId
    forM_ mGenBlock $ \createdBlk -> do
      logInfo $ sformat ("Created genesis block:\n" %build) createdBlk
      jlLog $ jlCreatedBlock (Left createdBlk)
    logDebug "Finished `processNewSlot`"

    fork_ $ do
        logWarningWaitLinear 8 "mpcOnNewSlot" $ untag @ssc sscOnNewSlot slotId
        logDebug "Finished `mpcOnNewSlot`"
    blkOnNewSlot @ssc slotId
    logDebug "Finished `blkOnNewSlot`"
