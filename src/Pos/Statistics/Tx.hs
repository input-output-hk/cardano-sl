-- | Helpers for benchmarking transactions

module Pos.Statistics.Tx
       ( statlogReceivedTx
       , statlogSentTx
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

statlogReceivedTx, statlogSentTx :: WorkMode m => Tx -> m ()
statlogReceivedTx = logStatM "received_transaction" . logTx
statlogSentTx = logStatM "sent_transaction" . logTx
