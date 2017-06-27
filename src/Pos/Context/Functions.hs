-- | Functions operating on NodeContext.

module Pos.Context.Functions
       (
         -- * Genesis
         GenesisUtxo(..)
       , genesisUtxoM
       , genesisStakesM
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

import           Universum

import           Control.Lens        (views)
import           Data.Time           (diffUTCTime, getCurrentTime)
import           Data.Time.Units     (Microsecond, fromMicroseconds)
import           EtherCompat

import           Pos.Context.Context (BlkSemaphore (..), GenesisStakes (..),
                                      GenesisUtxo (..), StartTime (..))
import           Pos.Core            (HeaderHash, SlotLeaders, StakesMap)
import           Pos.Genesis         (genesisLeaders)
import           Pos.Lrc.Context     (lrcActionOnEpoch, lrcActionOnEpochReason, waitLrc)
import           Pos.Txp.Toil.Types  (Utxo)

----------------------------------------------------------------------------
-- Genesis
----------------------------------------------------------------------------

genesisUtxoM :: (Functor m, MonadReader ctx m, HasLens GenesisUtxo ctx GenesisUtxo) => m Utxo
genesisUtxoM = views (lensOf @GenesisUtxo) unGenesisUtxo

genesisStakesM :: (Functor m, MonadReader ctx m, HasLens GenesisStakes ctx GenesisStakes) => m StakesMap
genesisStakesM = views (lensOf @GenesisStakes) unGenesisStakes

genesisLeadersM :: (Functor m, MonadReader ctx m, HasLens GenesisStakes ctx GenesisStakes) => m SlotLeaders
genesisLeadersM = genesisLeaders <$> genesisStakesM

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
