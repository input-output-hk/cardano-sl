{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies  #-}

-- | Helpers for benchmarking transactions

module Pos.Statistics.Tx
       ( StatProcessTx (..)
       -- , statlogReceivedTx
       -- , statlogSentTx
       ) where

import           Control.TimeWarp.Timed    (currentTime, runTimedIO)
import           Data.Binary               (Binary)
import qualified Data.Binary               as Binary
import           Data.Hashable             (Hashable (hashWithSalt))
import           Data.MessagePack          (MessagePack (fromObject, toObject),
                                            Object (..))
import           Data.SafeCopy             (SafeCopy (..))
import           Data.Text.Buildable       (build)
import           Serokell.Util             (show')
import           Universum

import           Pos.Crypto.Hashing        (hash)
import           Pos.Statistics.MonadStats (MonadStats (..))
import           Pos.Statistics.StatEntry  (CountStat, StatLabel (..))
import           Pos.Types                 (Timestamp (..), Tx)
import           Pos.WorkMode              (WorkMode)

data StatProcessTx = StatProcessTx deriving (Show, Eq, Generic, Typeable)

instance Binary StatProcessTx
instance MessagePack StatProcessTx
instance Hashable StatProcessTx

instance Buildable StatProcessTx where
    build _ = "stats_process_tx"

instance StatLabel StatProcessTx where
    type EntryType StatProcessTx = CountStat

-- logTx :: WorkMode m => Tx -> m StatEntry
-- logTx tx = do
--     ts <- Timestamp <$> liftIO (runTimedIO currentTime)
--     return (Binary.encode $ hash tx, ts)

-- statlogReceivedTx, statlogSentTx :: WorkMode m => Tx -> m ()
-- statlogReceivedTx = logStatM "received_transaction" . logTx
-- statlogSentTx = logStatM "sent_transaction" . logTx
