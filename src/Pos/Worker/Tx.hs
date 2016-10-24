-- | Tx processing related workers.

module Pos.Worker.Tx
       ( txWorkers
       ) where

import           Control.Lens              (ix, (^.), (^?))
import           Control.TimeWarp.Logging  (logInfo, logWarning)
import           Control.TimeWarp.Timed    (Microsecond, for, minute, repeatForever, wait)
import           Formatting                (build, sformat, (%))
import           Serokell.Util.Exceptions  ()
import           Universum

import           Pos.Communication.Methods (announceBlock, announceTxs)
import           Pos.Constants             (networkDiameter, slotDuration)
import           Pos.State                 (getHeadBlock, getLeaders, getLocalTxns)
import           Pos.Types                 (SlotId (..), gbHeader, slotIdF)
import           Pos.WorkMode              (WorkMode, getNodeContext, ncPublicKey)

-- | All workers specific to tx processing.
-- Exceptions:
-- 1. Worker which ticks when new slot starts.
txWorkers :: WorkMode m => [m ()]
txWorkers = [txsTransmitter]

txsTransmitterInterval :: Microsecond
txsTransmitterInterval = minute 1

txsTransmitter :: WorkMode m => m ()
txsTransmitter =
    repeatForever txsTransmitterInterval onError $
    do localTxs <- getLocalTxns
       announceTxs $ toList localTxs
  where
    sendToAll = notImplemented
    onError e =
        txsTransmitterInterval <$
        logWarning (sformat ("Error occured in txsTransmitter: " %build) e)
