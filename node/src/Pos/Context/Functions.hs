-- | Functions operating on NodeContext.

module Pos.Context.Functions
       (
         -- * Genesis
         GenesisUtxo(..)
       , genesisUtxoM
       , genesisStakesM
       , genesisLeadersM
       , genesisBlock0M

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
import           Ether.Internal      (HasLens (..))

import           Pos.Block.Core      (GenesisBlock, mkGenesisBlock)
import           Pos.Context.Context (BlkSemaphore (..), StartTime (..))
import           Pos.Core            (HasCoreConstants, HeaderHash, SlotLeaders,
                                      StakesMap)
import           Pos.Genesis         (GenesisUtxo (..), genesisLeaders)
import           Pos.Lrc.Context     (lrcActionOnEpoch, lrcActionOnEpochReason, waitLrc)
import           Pos.Ssc.Class       (SscHelpersClass)
import           Pos.Txp.Toil        (utxoToStakes)

----------------------------------------------------------------------------
-- Genesis
----------------------------------------------------------------------------

genesisUtxoM ::
       (Functor m, MonadReader ctx m, HasLens GenesisUtxo ctx GenesisUtxo)
    => m GenesisUtxo
genesisUtxoM = view (lensOf @GenesisUtxo)

genesisStakesM ::
       (Functor m, MonadReader ctx m, HasLens GenesisUtxo ctx GenesisUtxo)
    => m StakesMap
genesisStakesM = views (lensOf @GenesisUtxo) $ utxoToStakes . unGenesisUtxo

genesisLeadersM ::
       (Functor m, MonadReader ctx m, HasLens GenesisUtxo ctx GenesisUtxo, HasCoreConstants)
    => m SlotLeaders
genesisLeadersM = genesisLeaders <$> genesisUtxoM

genesisBlock0M ::
    forall ssc ctx m. ( Functor m, MonadReader ctx m, HasLens GenesisUtxo ctx GenesisUtxo
                      , HasCoreConstants, SscHelpersClass ssc)
    => m (GenesisBlock ssc)
genesisBlock0M = mkGenesisBlock @ssc Nothing 0 <$> genesisLeadersM

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
