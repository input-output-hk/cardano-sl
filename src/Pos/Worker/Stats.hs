module Pos.Worker.Stats
       ( statsWorkers
       ) where

import           Control.TimeWarp.Logging (logWarning)
import           Control.TimeWarp.Timed   (Microsecond, MonadTimed, currentTime,
                                           repeatForever, runTimedIO, sec)
import           Formatting               (build, sformat, (%))
import           Serokell.Util.Exceptions ()
import           Universum

import           Pos.Constants            (slotDuration)
import           Pos.Statistics           (StatBlockCreated (..), StatProcessTx (..),
                                           resetStat)
import           Pos.Types                (Timestamp (..))
import           Pos.WorkMode             (WorkMode)

txStatsRefreshInterval :: Microsecond
txStatsRefreshInterval = sec 1

curTime :: (MonadIO m, MonadTimed m) => m Timestamp
curTime = liftIO $ Timestamp <$> runTimedIO currentTime

statsWorkers :: WorkMode m => [m ()]
statsWorkers = [txStatsWorker, blockStatsWorker]

txStatsWorker :: WorkMode m => m ()
txStatsWorker =
    repeatForever txStatsRefreshInterval onError $ do
        ts <- curTime
        resetStat StatProcessTx ts
  where
    onError e = txStatsRefreshInterval <$
                logWarning (sformat ("Error occured in txStatsWorker: "%build) e)

blockStatsWorker :: WorkMode m => m ()
blockStatsWorker =
    repeatForever slotDuration onError $ do
        ts <- curTime
        resetStat StatBlockCreated ts
  where
    onError e = slotDuration <$
                logWarning (sformat ("Error occured in blockStatsWorker: "%build) e)
