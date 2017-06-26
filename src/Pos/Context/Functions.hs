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

import           Control.Lens        (views)
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

genesisUtxoM :: (Functor m, MonadReader ctx m, HasLens GenesisUtxo ctx GenesisUtxo) => m Utxo
genesisUtxoM = views (lensOf @GenesisUtxo) unGenesisUtxo

genesisLeadersM :: (Functor m, MonadReader ctx m, HasLens GenesisLeaders ctx GenesisLeaders) => m SlotLeaders
genesisLeadersM = views (lensOf @GenesisLeaders) unGenesisLeaders

----------------------------------------------------------------------------
-- Semaphore-related logic
----------------------------------------------------------------------------

takeBlkSemaphore
    :: (MonadIO m, MonadReader ctx m, HasLens BlkSemaphore ctx BlkSemaphore)
    => m HeaderHash
takeBlkSemaphore = takeMVar =<< views (lensOf @BlkSemaphore) unBlkSemaphore

putBlkSemaphore
    :: (MonadIO m, MonadReader ctx m, HasLens BlkSemaphore ctx BlkSemaphore)
    => HeaderHash -> m ()
putBlkSemaphore tip = flip putMVar tip =<< views (lensOf @BlkSemaphore) unBlkSemaphore

readBlkSemaphore
    :: (MonadIO m, MonadReader ctx m, HasLens BlkSemaphore ctx BlkSemaphore)
    => m HeaderHash
readBlkSemaphore = readMVar =<< views (lensOf @BlkSemaphore) unBlkSemaphore

----------------------------------------------------------------------------
-- Misc
----------------------------------------------------------------------------

-- | Returns node uptime based on current time and 'StartTime'.
getUptime :: (MonadIO m, MonadReader ctx m, HasLens StartTime ctx StartTime) => m Microsecond
getUptime = do
    curTime <- liftIO getCurrentTime
    startTime <- views (lensOf @StartTime) unStartTime
    let seconds = toRational $ curTime `diffUTCTime` startTime
    pure $ fromMicroseconds $ round $ seconds * 1000 * 1000
