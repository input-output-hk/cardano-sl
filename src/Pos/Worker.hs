-- | High level workers.

module Pos.Worker
       ( runWorkers
       , statsWorkers
       ) where

import           Data.Tagged             (untag)
import           Data.Time.Units         (convertUnit)
import           Formatting              (sformat, (%))
import           Mockable                (fork)
import           Node                    (SendActions, Worker)
import           System.Wlog             (logInfo, logNotice)
import           Universum

import           Pos.Block.Worker        (blkWorkers)
import           Pos.Communication       (BiP, SysStartResponse (..))
import           Pos.Communication.Types (VerInfo)
import           Pos.Constants           (sysTimeBroadcastSlots)
import           Pos.Context             (NodeContext (..), getNodeContext,
                                          setNtpLastSlot)
import           Pos.DHT.Model.Neighbors (sendToNeighbors)
import           Pos.DHT.Workers         (dhtWorkers)
import           Pos.Lrc.Worker          (lrcOnNewSlotWorker)
import           Pos.Security.Workers    (SecurityWorkersClass, securityWorkers)
import           Pos.Slotting            (getSlotDuration, onNewSlotWithLogging)
import           Pos.Ssc.Class.Workers   (SscWorkersClass, sscWorkers)
import           Pos.Types               (SlotId, flattenSlotId, slotIdF)
import           Pos.Update              (usWorkers)
import           Pos.Util                (waitRandomInterval, withWaitLog)
import           Pos.Util.TimeWarp       (ms)
import           Pos.Worker.Stats        (statsWorkers)
import           Pos.WorkMode            (WorkMode)

-- | Run all necessary workers in separate threads. This call doesn't
-- block.
--
-- A note about order: currently all onNewSlot updates can be run
-- in parallel and we try to maintain this rule. If at some point
-- order becomes important, update this comment! I don't think you
-- will read it, but who knows…
runWorkers
    :: (SscWorkersClass ssc, SecurityWorkersClass ssc, WorkMode ssc m)
    => Worker BiP VerInfo m
runWorkers sendActions = mapM_ fork $ map ($ withWaitLog sendActions) $ concat
    [ [ onNewSlotWorker ]
    , dhtWorkers
    , blkWorkers
    , untag sscWorkers
    , untag securityWorkers
    , [lrcOnNewSlotWorker]
    , usWorkers
    ]

onNewSlotWorker :: WorkMode ssc m => Worker BiP VerInfo m
onNewSlotWorker sendActions = onNewSlotWithLogging True $ onNewSlotWorkerImpl sendActions

onNewSlotWorkerImpl :: WorkMode ssc m =>  SendActions BiP VerInfo m -> SlotId -> m ()
onNewSlotWorkerImpl sendActions slotId = do
    logNotice $ sformat ("New slot has just started: "%slotIdF) slotId
    setNtpLastSlot slotId
    when (flattenSlotId slotId <= sysTimeBroadcastSlots) $
      whenM (ncTimeLord <$> getNodeContext) $ void $ fork $ do
        let send = do sysStart <- ncSystemStart <$> getNodeContext
                      logInfo "Broadcasting system start"
                      sendToNeighbors sendActions $ SysStartResponse sysStart
        send
        slotDuration <- getSlotDuration
        waitRandomInterval (ms 500) (convertUnit slotDuration `div` 2)
        send
