{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies  #-}

-- | Helpers for benchmarking transactions

module Pos.Statistics.Tx
       ( StatProcessTx (..)
       -- , statlogReceivedTx
       -- , statlogSentTx
       ) where

import           Control.Monad            (fail)
import           Data.Binary              (Binary)
import qualified Data.Binary              as Binary
import qualified Data.Binary.Get          as Binary (getWord32be)
import qualified Data.Binary.Put          as Binary (putWord32be)
import           Data.Hashable            (Hashable (hash, hashWithSalt))
import           Data.MessagePack         (MessagePack (fromObject, toObject))
import           Data.Text.Buildable      (build)
import           Universum

import           Pos.Statistics.StatEntry (CountStat, StatLabel (..))
import           Pos.Util                 (fromMsgpackBinary, toMsgpackBinary)

data StatProcessTx = StatProcessTx deriving (Show, Eq, Generic, Typeable)

-- TODO: generate these using TH
instance Buildable StatProcessTx where
    build _ = "StatProcessTx"

hId :: Word32
hId = fromIntegral $ hash ("StatProcessTx" :: ByteString)

instance Binary StatProcessTx where
    get = do
        w <- Binary.getWord32be
        when (w /= hId) $
            fail "Binary.get: StatProcessTx fail"
        return StatProcessTx
    put _ = Binary.putWord32be hId

instance MessagePack StatProcessTx where
    toObject = toMsgpackBinary
    fromObject = fromMsgpackBinary "StatProcessTx"

instance Hashable StatProcessTx where
    hashWithSalt x _ = hashWithSalt x ("StatProcessTx" :: ByteString)

instance StatLabel StatProcessTx where
    type EntryType StatProcessTx = CountStat
    labelName _ = "StatProcessTx"

-- logTx :: WorkMode m => Tx -> m StatEntry
-- logTx tx = do
--     ts <- Timestamp <$> liftIO (runTimedIO currentTime)
--     return (Binary.encode $ hash tx, ts)

-- statlogReceivedTx, statlogSentTx :: WorkMode m => Tx -> m ()
-- statlogReceivedTx = logStatM "received_transaction" . logTx
-- statlogSentTx = logStatM "sent_transaction" . logTx
