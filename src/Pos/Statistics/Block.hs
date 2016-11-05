{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies  #-}

-- | Helpers for benchmarking blocks distribution

module Pos.Statistics.Block
       ( StatBlockVerifying (..)
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

import           Pos.Statistics.StatEntry (StatLabel (..), ValueStat)
import           Pos.Util                 (fromMsgpackBinary, toMsgpackBinary)

data StatBlockVerifying = StatBlockVerifying deriving (Show, Eq, Generic, Typeable)

-- TODO: generate these using TH
instance Buildable StatBlockVerifying where
    build _ = "StatBlockVerifying"

instance Hashable StatBlockVerifying where
    hashWithSalt x _ = hashWithSalt x ("StatBlockVerifying" :: ByteString)

hId :: Word32
hId = fromIntegral $ hash ("StatBlockVerifying" :: ByteString)

instance Binary StatBlockVerifying where
    get = do
        w <- Binary.getWord32be
        when (w /= hId) $
            fail "Binary.get: StatBlockVerifying fail"
        return StatBlockVerifying
    put _ = Binary.putWord32be hId

instance MessagePack StatBlockVerifying where
    toObject = toMsgpackBinary
    fromObject = fromMsgpackBinary "StatBlockVerifying"

instance StatLabel StatBlockVerifying where
    type EntryType StatBlockVerifying = ValueStat
    labelName _ = "StatBlockVerifying"

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
