-- | Helpers for benchmarking blocks distribution

module Pos.Statistics.Block
       ( statlogReceivedBlockHeader
       , statlogSentBlockHeader
       , statlogReceivedBlock
       , statlogSentBlock
       ) where

import           Control.TimeWarp.Timed    (currentTime, runTimedIO)
import qualified Data.Binary               as Binary
import           Universum

import           Pos.Crypto.Hashing        (hash)
import           Pos.Ssc.Class.Types       (SscTypes)
import           Pos.Statistics.MonadStats (MonadStats (..), StatEntry)
import           Pos.Types                 (Block, BlockHeader, Timestamp (..),
                                            getBlockHeader)
import           Pos.WorkMode              (WorkMode)

logBlockHeader
    :: (SscTypes ssc, WorkMode m)
    => BlockHeader ssc -> m StatEntry
logBlockHeader header = do
    ts <- Timestamp <$> liftIO (runTimedIO currentTime)
    return (Binary.encode $ hash header, ts)

statlogReceivedBlockHeader, statlogSentBlockHeader
    :: (SscTypes ssc, WorkMode m)
    => BlockHeader ssc -> m ()
statlogReceivedBlockHeader = logStatM "received_block_header" . logBlockHeader
statlogSentBlockHeader = logStatM "sent_block_header" . logBlockHeader

logBlock :: (SscTypes ssc, WorkMode m) => Block ssc -> m StatEntry
logBlock = logBlockHeader . getBlockHeader

statlogReceivedBlock, statlogSentBlock
    :: (SscTypes ssc, WorkMode m)
    => Block ssc -> m ()
statlogReceivedBlock = logStatM "received_block_body" . logBlock
statlogSentBlock = logStatM "sent_log_body" . logBlock
