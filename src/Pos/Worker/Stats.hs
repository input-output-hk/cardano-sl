module Pos.Worker.Stats
       ( statsWorker
       ) where

import           Control.TimeWarp.Logging  (logInfo, logWarning)
import           Control.TimeWarp.Timed    (Microsecond, currentTime, repeatForever,
                                            runTimedIO, sec)
import           Formatting                (build, sformat, (%))
import           Serokell.Util.Exceptions  ()
import           Universum

import           Pos.Communication.Methods (announceTxs)
import           Pos.State                 (getLocalTxs)
import           Pos.Statistics            (StatProcessTx (..), resetStat)
import           Pos.Types                 (Timestamp (..))
import           Pos.WorkMode              (WorkMode)

statsRefreshInterval :: Microsecond
statsRefreshInterval = sec 1

statsWorker :: WorkMode m => m ()
statsWorker =
    repeatForever statsRefreshInterval onError $ do
        ts <- liftIO $ Timestamp <$> runTimedIO currentTime
        logInfo "RESET STATS"
        resetStat StatProcessTx ts
  where
    onError e = statsRefreshInterval <$
                logWarning (sformat ("Error occured in statsWorker: "%build) e)
