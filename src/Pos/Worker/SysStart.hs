-- | System start worker.

module Pos.Worker.SysStart
       ( sysStartWorker
       ) where

import           Data.Proxy              (Proxy (..))
import           Data.Time.Units         (convertUnit)
import           Mockable                (fork)
import           System.Wlog             (logInfo)
import           Universum

import           Pos.Communication       (OutSpecs, SysStartResponse (..), WorkerSpec,
                                          localWorker, onNewSlotWithLoggingWorker,
                                          oneMsgH, toOutSpecs)
import           Pos.Constants           (isDevelopment, sysTimeBroadcastSlots)
import           Pos.Context             (NodeContext (..), getNodeContext, npSystemStart,
                                          npTimeLord)
import           Pos.DHT.Model.Neighbors (sendToNeighbors)
import           Pos.Types               (flattenSlotId)
import           Pos.Util                (waitRandomInterval)
import           Pos.Util.TimeWarp       (ms)
import           Pos.WorkMode            (WorkMode)

sysStartWorker
    :: WorkMode ssc m
    => (WorkerSpec m, OutSpecs)
sysStartWorker
    | not isDevelopment = localWorker pass
    | otherwise =
        onNewSlotWithLoggingWorker True outs $ \slotId sendActions -> do
            when (flattenSlotId slotId <= sysTimeBroadcastSlots) $
                whenM (npTimeLord . ncNodeParams <$> getNodeContext) $
                void $
                fork $ do
                    let send = do
                            sysStart <- npSystemStart . ncNodeParams <$> getNodeContext
                            logInfo "Broadcasting system start"
                            sendToNeighbors sendActions $
                                SysStartResponse sysStart
                    send
                    -- slotDuration <- getSlotDuration
                    waitRandomInterval
                        (ms 500)
                        -- (convertUnit slotDuration `div` 2)
                        undefined
                    send
  where
    outs = toOutSpecs [oneMsgH (Proxy :: Proxy SysStartResponse)]
