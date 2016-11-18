{-# LANGUAGE ViewPatterns #-}
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

import           Pos.Communication        (SysStartResponse (..))
import           Pos.Constants            (sysTimeBroadcastSlots)
import           Pos.DHT                  (sendToNetwork)
import           Pos.Slotting             (onNewSlot)
import           Pos.Ssc.Class.Workers    (SscWorkersClass, sscOnNewSlot, sscWorkers)
import           Pos.State                (processNewSlot)
import           Pos.Types                (SlotId, flattenSlotId, slotIdF)
import           Pos.Util.JsonLog         (jlCreatedBlock, jlLog)
import           Pos.Worker.Block         (blkOnNewSlot, blkWorkers)
import           Pos.Worker.Stats         (statsWorkers)
import           Pos.Worker.Tx            (txWorkers)
import           Pos.WorkMode             (NodeContext (..), WorkMode, getNodeContext)

-- | Run all necessary workers in separate threads. This call doesn't
-- block.
runWorkers :: (SscWorkersClass ssc,  WorkMode ssc m) => m ()
runWorkers = mapM_ fork_ $ concat
    [ [onNewSlotWorker]
    , blkWorkers
    , untag sscWorkers
    , txWorkers
    ]

onNewSlotWorker :: (SscWorkersClass ssc, WorkMode ssc m) => m ()
onNewSlotWorker = onNewSlot True onNewSlotWorkerImpl

onNewSlotWorkerImpl :: (SscWorkersClass ssc, WorkMode ssc m)
                    => SlotId -> m ()
onNewSlotWorkerImpl slotId = do
    logNotice $ sformat ("New slot has just started: "%slotIdF) slotId
    -- A note about order: currently only one thing is important, that
    -- `processNewSlot` is executed before everything else
    mGenBlock <- processNewSlot slotId
    forM_ mGenBlock $ \createdBlk -> do
      logInfo $ sformat ("Created genesis block:\n" %build) createdBlk
      jlLog $ jlCreatedBlock (Left createdBlk)
    logDebug "Finished `processNewSlot`"

    when (flattenSlotId slotId <= sysTimeBroadcastSlots) $
      whenM (ncTimeLord <$> getNodeContext) $
        ncSystemStart <$> getNodeContext
            >>= \(SysStartResponse . Just -> mT) -> do
                logInfo "Broadcasting system start"
                sendToNetwork mT

    fork_ (untag sscOnNewSlot slotId)
    blkOnNewSlot slotId
    logDebug "Finished `blkOnNewSlot`"
