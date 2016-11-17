{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies  #-}

-- | Helpers for benchmarking blocks distribution

module Pos.Statistics.Block
       ( StatBlockCreated (..)
       -- , statlogReceivedBlockHeader
       -- , statlogSentBlockHeader
       -- , statlogReceivedBlock
       -- , statlogSentBlock
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

data StatBlockCreated = StatBlockCreated deriving (Show, Eq, Generic, Typeable)

-- TODO: generate these using TH
instance Buildable StatBlockCreated where
    build _ = "StatBlockCreated"

instance Hashable StatBlockCreated where
    hashWithSalt x _ = hashWithSalt x ("StatBlockCreated" :: ByteString)

hId :: Word32
hId = fromIntegral $ hash ("StatBlockCreated" :: ByteString)

instance Binary StatBlockCreated where
    get = do
        w <- Binary.getWord32be
        when (w /= hId) $
            fail "Binary.get: StatBlockCreated fail"
        return StatBlockCreated
    put _ = Binary.putWord32be hId

instance MessagePack StatBlockCreated where
    toObject = toMsgpackBinary
    fromObject = fromMsgpackBinary "StatBlockCreated"

instance StatLabel StatBlockCreated where
    type EntryType StatBlockCreated = CountStat
    labelName _ = "StatBlockCreated"

-- logBlockHeader
--     :: (SscTypes ssc, WorkMode m)
--     => BlockHeader ssc -> m StatEntry
-- logBlockHeader header = do
--     ts <- Timestamp <$> liftIO (runTimedIO currentTime)
--     return (Binary.encode $ hash header, ts)

-- statlogReceivedBlockHeader, statlogSentBlockHeader
--     :: (SscTypes ssc, WorkMode m)
--     => BlockHeader ssc -> m ()
-- statlogReceivedBlockHeader = logStatM "received_block_header" . logBlockHeader
-- statlogSentBlockHeader = logStatM "sent_block_header" . logBlockHeader

-- logBlock :: (SscTypes ssc, WorkMode m) => Block ssc -> m StatEntry
-- logBlock = logBlockHeader . getBlockHeader

-- statlogReceivedBlock, statlogSentBlock
--     :: (SscTypes ssc, WorkMode m)
--     => Block ssc -> m ()
-- statlogReceivedBlock = logStatM "received_block_body" . logBlock
-- statlogSentBlock = logStatM "sent_log_body" . logBlock
