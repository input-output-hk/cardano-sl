-- | High level workers.

module Pos.Worker
       ( runWorkers
       , statsWorkers
       ) where

import           Control.TimeWarp.Timed (ms)
import           Data.Tagged            (untag)
import           Formatting             (sformat, (%))
import           Mockable               (fork)
import           Node                   (SendActions)
import           System.Wlog            (logInfo, logNotice)
import           Universum

import           Pos.Block.Worker       (blkWorkers)
import           Pos.Communication      (BiP, SysStartResponse (..))
import           Pos.Constants          (slotDuration, sysTimeBroadcastSlots)
import           Pos.Context            (NodeContext (..), getNodeContext)
import           Pos.Lrc.Worker         (lrcOnNewSlotWorker)
import           Pos.NewDHT.Model       (sendToNeighbors)
import           Pos.Security.Workers   (SecurityWorkersClass, securityWorkers)
import           Pos.Slotting           (onNewSlot')
import           Pos.Ssc.Class.Workers  (SscWorkersClass, sscWorkers)
import           Pos.Types              (SlotId, flattenSlotId, slotIdF)
import           Pos.Update             (usWorkers)
import           Pos.Util               (waitRandomInterval')
import           Pos.Worker.Stats       (statsWorkers)
import           Pos.WorkMode           (NewWorkMode)

-- | Run all necessary workers in separate threads. This call doesn't
-- block.
--
-- A note about order: currently all onNewSlot updates can be run
-- in parallel and we try to maintain this rule. If at some point
-- order becomes important, update this comment! I don't think you
-- will read it, but who knows…
--runWorkers :: (SscWorkersClass ssc, NewWorkMode ssc m) => m ()
runWorkers :: (SscWorkersClass ssc, SecurityWorkersClass ssc, NewWorkMode ssc m) => SendActions BiP m -> m ()
runWorkers sendActions = mapM_ fork $ map ($ sendActions) $ concat
    [ [ onNewSlot' True . onNewSlotWorkerImpl ]
    , blkWorkers
    , untag sscWorkers
    , untag securityWorkers
    , [lrcOnNewSlotWorker]
    , usWorkers
    ]

onNewSlotWorkerImpl :: NewWorkMode ssc m => SendActions BiP m -> SlotId -> m ()
onNewSlotWorkerImpl sendActions slotId = do
    logNotice $ sformat ("New slot has just started: "%slotIdF) slotId
    when (flattenSlotId slotId <= sysTimeBroadcastSlots) $
      whenM (ncTimeLord <$> getNodeContext) $ void $ fork $ do
        let send = ncSystemStart <$> getNodeContext
                    >>= \sysStart -> do
                        logInfo "Broadcasting system start"
                        sendToNeighbors sendActions $ SysStartResponse sysStart
        send
        waitRandomInterval' (ms 500) (slotDuration `div` 2)
        send
