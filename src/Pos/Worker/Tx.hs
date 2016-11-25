{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}

-- | Tx processing related workers.

module Pos.Worker.Tx
       ( txWorkers
       ) where

import           Control.TimeWarp.Timed    (Microsecond, repeatForever)
import           Formatting                (build, sformat, (%))
import           Serokell.Util.Exceptions  ()
import           System.Wlog               (logWarning)
import           Universum

import           Pos.Communication.Methods (announceTxs)
import           Pos.Constants             (slotDuration)
import           Pos.State                 (getLocalTxs)
import           Pos.WorkMode              (WorkMode)

-- | All workers specific to tx processing.
-- Exceptions:
-- 1. Worker which ticks when new slot starts.
txWorkers :: WorkMode ssc m => [m ()]
txWorkers = [txsTransmitter]

txsTransmitterInterval :: Microsecond
txsTransmitterInterval = slotDuration

txsTransmitter :: WorkMode ssc m => m ()
txsTransmitter =
    repeatForever txsTransmitterInterval onError $
    do localTxs <- getLocalTxs
       announceTxs $ toList localTxs
  where
    onError e =
        txsTransmitterInterval <$
        logWarning (sformat ("Error occured in txsTransmitter: " %build) e)
