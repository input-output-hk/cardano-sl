-- | High level workers.

module Pos.Worker
       ( runWorkers
       , statsWorkers
       ) where

import           Control.TimeWarp.Timed (fork_, ms)
import           Data.Tagged            (untag)
import           Formatting             (sformat, (%))
import           System.Wlog            (logInfo, logNotice)
import           Universum

import           Pos.Block.Worker       (blkWorkers)
import           Pos.Communication      (SysStartResponse (..))
import           Pos.Constants          (slotDuration, sysTimeBroadcastSlots)
import           Pos.Context            (NodeContext (..), getNodeContext)
import           Pos.DHT.Model          (sendToNetwork)
import           Pos.Security.Workers   (SecurityWorkersClass, securityWorkers)
import           Pos.Slotting           (onNewSlot)
import           Pos.Ssc.Class.Workers  (SscWorkersClass, sscWorkers)
import           Pos.Types              (SlotId, flattenSlotId, slotIdF)
import           Pos.Update             (usWorkers)
import           Pos.Util               (waitRandomInterval)
import           Pos.Worker.Lrc         (lrcOnNewSlotWorker)
import           Pos.Worker.Stats       (statsWorkers)
import           Pos.WorkMode           (WorkMode)

-- | Run all necessary workers in separate threads. This call doesn't
-- block.
--
-- A note about order: currently all onNewSlot updates can be run
-- in parallel and we try to maintain this rule. If at some point
-- order becomes important, update this comment! I don't think you
-- will read it, but who knows…
runWorkers :: (SscWorkersClass ssc, SecurityWorkersClass ssc, WorkMode ssc m) => m ()
runWorkers = mapM_ fork_ $ concat
    [ [onNewSlotWorker]
    , blkWorkers
    , untag sscWorkers
    , untag securityWorkers
    , [lrcOnNewSlotWorker]
    , usWorkers
    ]

onNewSlotWorker :: WorkMode ssc m => m ()
onNewSlotWorker = onNewSlot True onNewSlotWorkerImpl

onNewSlotWorkerImpl :: WorkMode ssc m => SlotId -> m ()
onNewSlotWorkerImpl slotId = do
    logNotice $ sformat ("New slot has just started: "%slotIdF) slotId
    when (flattenSlotId slotId <= sysTimeBroadcastSlots) $
      whenM (ncTimeLord <$> getNodeContext) $ fork_ $ do
        let send = ncSystemStart <$> getNodeContext
                    >>= \sysStart -> do
                        logInfo "Broadcasting system start"
                        sendToNetwork $ SysStartResponse sysStart (Just slotId)
        send
        waitRandomInterval (ms 500) (slotDuration `div` 2)
        send
