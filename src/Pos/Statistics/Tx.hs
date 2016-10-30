-- | Helpers for benchmarking transactions

module Pos.Statistics.Tx
       ( logReceivedTx
       , logSentTx
       ) where

import           Control.TimeWarp.Timed    (currentTime, runTimedIO)
import qualified Data.Binary               as Binary
import           Universum

import           Pos.Crypto.Hashing        (hash)
import           Pos.Statistics.MonadStats (MonadStats (..), StatEntry)
import           Pos.Types                 (Timestamp (..), Tx)
import           Pos.WorkMode              (WorkMode)

logTx :: WorkMode m => Tx -> m StatEntry
logTx tx = do
    ts <- Timestamp <$> liftIO (runTimedIO currentTime)
    return (Binary.encode $ hash tx, ts)

logReceivedTx, logSentTx :: WorkMode m => Tx -> m ()
logReceivedTx = logStatM "received_transaction" . logTx
logSentTx = logStatM "sent_transaction" . logTx
