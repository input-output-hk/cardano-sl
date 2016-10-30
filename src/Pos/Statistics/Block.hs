-- | Helpers for benchmarking blocks distribution

module Pos.Statistics.Block
       ( logReceivedBlockHeader
       , logSentBlockHeader
       , logReceivedBlock
       , logSentBlock
       ) where

import           Control.TimeWarp.Timed    (currentTime, runTimedIO)
import qualified Data.Binary               as Binary
import           Universum

import           Pos.Crypto.Hashing        (hash)
import           Pos.Statistics.MonadStats (MonadStats (..), StatEntry)
import           Pos.Types                 (Block, BlockHeader, Timestamp (..),
                                            getBlockHeader)
import           Pos.WorkMode              (WorkMode)

logBlockHeader :: WorkMode m => BlockHeader -> m StatEntry
logBlockHeader header = do
    ts <- Timestamp <$> liftIO (runTimedIO currentTime)
    return (Binary.encode $ hash header, ts)

logReceivedBlockHeader, logSentBlockHeader :: WorkMode m => BlockHeader -> m ()
logReceivedBlockHeader = logStatM "received_block_header" . logBlockHeader
logSentBlockHeader = logStatM "sent_block_header" . logBlockHeader

logBlock :: WorkMode m => Block -> m StatEntry
logBlock = logBlockHeader . getBlockHeader

logReceivedBlock, logSentBlock :: WorkMode m => Block -> m ()
logReceivedBlock = logStatM "received_block_body" . logBlock
logSentBlock = logStatM "sent_log_body" . logBlock
