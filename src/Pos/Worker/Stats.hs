-- | Workers for collecting transaction statistics.

module Pos.Worker.Stats
       ( statsWorkers
       ) where

import           Data.Time.Units            (Microsecond)
import           Formatting                 (build, sformat, (%))
import           Mockable                   (delay)
import           Pos.Communication.Protocol (OutSpecs, WorkerSpec, localWorker)
import           Serokell.Util              (sec)
import           System.Wlog                (logWarning)
import           Universum



import           Pos.Statistics             (StatProcessTx (..), resetStat)
import           Pos.WorkMode.Class         (WorkMode)

txStatsRefreshInterval :: Microsecond
txStatsRefreshInterval = sec 1

-- | Workers for collecting statistics about transactions in background.
statsWorkers :: WorkMode ssc m => ([WorkerSpec m], OutSpecs)
statsWorkers = first pure txStatsWorker

txStatsWorker :: WorkMode ssc m => (WorkerSpec m, OutSpecs)
txStatsWorker = localWorker $ loop `catchAll` onError
  where
    loop = do
        resetStat StatProcessTx
        delay txStatsRefreshInterval
        loop
    onError e = do
        logWarning (sformat ("Error occured in txStatsWorker: "%build) e)
        delay txStatsRefreshInterval
        loop
