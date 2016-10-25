-- | Tx processing related workers.

module Pos.Worker.Tx
       ( txWorkers
       ) where

import           Control.TimeWarp.Logging  (logWarning)
import           Control.TimeWarp.Timed    (Microsecond, repeatForever, sec)
import           Formatting                (build, sformat, (%))
import           Serokell.Util.Exceptions  ()
import           Universum

import           Pos.Communication.Methods (announceTxs)
import           Pos.State                 (getLocalTxs)
import           Pos.WorkMode              (WorkMode)

-- | All workers specific to tx processing.
-- Exceptions:
-- 1. Worker which ticks when new slot starts.
txWorkers :: WorkMode m => [m ()]
txWorkers = [txsTransmitter]

txsTransmitterInterval :: Microsecond
txsTransmitterInterval = sec 2

txsTransmitter :: WorkMode m => m ()
txsTransmitter =
    repeatForever txsTransmitterInterval onError $
    do localTxs <- getLocalTxs
       announceTxs $ toList localTxs
  where
    onError e =
        txsTransmitterInterval <$
        logWarning (sformat ("Error occured in txsTransmitter: " %build) e)
