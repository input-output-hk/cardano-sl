-- | Workers for collecting transaction statistics.

module Pos.Worker.Stats
       ( statsWorkers
       ) where

import           Data.Time.Units          (Microsecond)
import           Formatting               (build, sformat, (%))
import           Mockable                 (delay)
import           Node                     (SendActions)
import           Pos.Util.TimeWarp        (sec)
import           Serokell.Util.Exceptions ()
import           System.Wlog              (logWarning)
import           Universum

import           Pos.Communication.BiP    (BiP)
import           Pos.Statistics           (StatProcessTx (..), resetStat)
import           Pos.WorkMode             (WorkMode)

txStatsRefreshInterval :: Microsecond
txStatsRefreshInterval = sec 1

-- | Workers for collecting statistics about transactions in background.
statsWorkers :: WorkMode ssc m => [SendActions BiP m -> m ()]
statsWorkers = [const txStatsWorker]

txStatsWorker :: WorkMode ssc m => m ()
txStatsWorker = loop `catchAll` onError
  where
    loop = do
        resetStat StatProcessTx
        delay txStatsRefreshInterval
        loop
    onError e = do
        logWarning (sformat ("Error occured in txStatsWorker: "%build) e)
        delay txStatsRefreshInterval
        loop
