-- | Workers for collecting transaction statistics.

module Pos.Worker.Stats
       ( statsWorkers
       ) where

import           Control.TimeWarp.Timed   (Microsecond, sec)
import           Formatting               (build, sformat, (%))
import           Mockable                 (delay, for)
import           Node                     (SendActions)
import           Serokell.Util.Exceptions ()
import           System.Wlog              (logWarning)
import           Universum

import           Pos.Communication.BiP    (BiP)
import           Pos.Statistics           (StatProcessTx (..), resetStat)
import           Pos.WorkMode             (NewWorkMode)

txStatsRefreshInterval :: Microsecond
txStatsRefreshInterval = sec 1

-- | Workers for collecting statistics about transactions in background.
statsWorkers :: NewWorkMode ssc m => [SendActions BiP m -> m ()]
statsWorkers = [const txStatsWorker]

txStatsWorker :: NewWorkMode ssc m => m ()
txStatsWorker = loop `catchAll` onError
  where
    loop = do
        resetStat StatProcessTx
        delay $ for txStatsRefreshInterval
        loop
    onError e = do
        logWarning (sformat ("Error occured in txStatsWorker: "%build) e)
        delay $ for txStatsRefreshInterval
        loop
