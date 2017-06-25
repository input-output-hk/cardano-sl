-- | Functions operating on NodeContext.

module Pos.Context.Functions
       (
         -- * Genesis
         GenesisUtxo(..)
       , genesisUtxoM
       , genesisLeadersM

         -- * Block semaphore.
       , putBlkSemaphore
       , readBlkSemaphore
       , takeBlkSemaphore

         -- * LRC synchronization
       , waitLrc
       , lrcActionOnEpoch
       , lrcActionOnEpochReason

         -- * Misc
       , getUptime
       ) where

import           Data.Time           (diffUTCTime, getCurrentTime)
import           Data.Time.Units     (Microsecond, fromMicroseconds)
import           EtherCompat
import           Universum

import           Pos.Context.Context (BlkSemaphore (..), GenesisLeaders (..),
                                      GenesisUtxo (..), StartTime (..))
import           Pos.Lrc.Context     (lrcActionOnEpoch, lrcActionOnEpochReason, waitLrc)
import           Pos.Txp.Toil.Types  (Utxo)
import           Pos.Types           (HeaderHash, SlotLeaders)

----------------------------------------------------------------------------
-- Genesis
----------------------------------------------------------------------------

genesisUtxoM :: (Functor m, MonadCtx ctx GenesisUtxo GenesisUtxo m) => m Utxo
genesisUtxoM = asksCtx @GenesisUtxo unGenesisUtxo

genesisLeadersM :: (Functor m, MonadCtx ctx GenesisLeaders GenesisLeaders m) => m SlotLeaders
genesisLeadersM = asksCtx @GenesisLeaders unGenesisLeaders

----------------------------------------------------------------------------
-- Semaphore-related logic
----------------------------------------------------------------------------

takeBlkSemaphore
    :: (MonadIO m, MonadCtx ctx BlkSemaphore BlkSemaphore m)
    => m HeaderHash
takeBlkSemaphore = takeMVar =<< asksCtx @BlkSemaphore unBlkSemaphore

putBlkSemaphore
    :: (MonadIO m, MonadCtx ctx BlkSemaphore BlkSemaphore m)
    => HeaderHash -> m ()
putBlkSemaphore tip = flip putMVar tip =<< asksCtx @BlkSemaphore unBlkSemaphore

readBlkSemaphore
    :: (MonadIO m, MonadCtx ctx BlkSemaphore BlkSemaphore m)
    => m HeaderHash
readBlkSemaphore = readMVar =<< asksCtx @BlkSemaphore unBlkSemaphore

----------------------------------------------------------------------------
-- Misc
----------------------------------------------------------------------------

-- | Returns node uptime based on current time and 'StartTime'.
getUptime :: (MonadIO m, MonadCtx ctx StartTime StartTime m) => m Microsecond
getUptime = do
    curTime <- liftIO getCurrentTime
    startTime <- asksCtx @StartTime unStartTime
    let seconds = toRational $ curTime `diffUTCTime` startTime
    pure $ fromMicroseconds $ round $ seconds * 1000 * 1000
