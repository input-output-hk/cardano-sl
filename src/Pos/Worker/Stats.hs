{-# LANGUAGE FlexibleContexts #-}

module Pos.Worker.Stats
       ( statsWorkers
       ) where

import           Control.TimeWarp.Timed   (Microsecond, repeatForever, sec)
import           Formatting               (build, sformat, (%))
import           Serokell.Util.Exceptions ()
import           System.Wlog              (logWarning)
import           Universum

import           Pos.Statistics           (StatProcessTx (..), resetStat)
import           Pos.WorkMode             (WorkMode)

txStatsRefreshInterval :: Microsecond
txStatsRefreshInterval = sec 1

statsWorkers :: WorkMode ssc m => [m ()]
statsWorkers = [txStatsWorker]

txStatsWorker :: WorkMode ssc m => m ()
txStatsWorker =
    repeatForever txStatsRefreshInterval onError $ resetStat StatProcessTx
  where
    onError e = txStatsRefreshInterval <$
                logWarning (sformat ("Error occured in txStatsWorker: "%build) e)
