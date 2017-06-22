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

import           Universum

import           Data.Time           (diffUTCTime, getCurrentTime)
import           Data.Time.Units     (Microsecond, fromMicroseconds)
import qualified Ether

import           Pos.Context.Context (BlkSemaphore (..), GenesisUtxo (..), StartTime (..))
import           Pos.Core            (HeaderHash, SlotLeaders)
import           Pos.Genesis         (genesisLeaders)
import           Pos.Lrc.Context     (lrcActionOnEpoch, lrcActionOnEpochReason, waitLrc)
import           Pos.Txp.Toil.Types  (Utxo)

----------------------------------------------------------------------------
-- Genesis
----------------------------------------------------------------------------

genesisUtxoM :: (Functor m, Ether.MonadReader' GenesisUtxo m) => m Utxo
genesisUtxoM = Ether.asks' unGenesisUtxo

genesisLeadersM :: (Functor m, Ether.MonadReader' GenesisUtxo m) => m SlotLeaders
genesisLeadersM = genesisLeaders <$> genesisUtxoM

----------------------------------------------------------------------------
-- Semaphore-related logic
----------------------------------------------------------------------------

takeBlkSemaphore
    :: (MonadIO m, Ether.MonadReader' BlkSemaphore m)
    => m HeaderHash
takeBlkSemaphore = takeMVar =<< Ether.asks' unBlkSemaphore

putBlkSemaphore
    :: (MonadIO m, Ether.MonadReader' BlkSemaphore m)
    => HeaderHash -> m ()
putBlkSemaphore tip = flip putMVar tip =<< Ether.asks' unBlkSemaphore

readBlkSemaphore
    :: (MonadIO m, Ether.MonadReader' BlkSemaphore m)
    => m HeaderHash
readBlkSemaphore = readMVar =<< Ether.asks' unBlkSemaphore

----------------------------------------------------------------------------
-- Misc
----------------------------------------------------------------------------

-- | Returns node uptime based on current time and 'StartTime'.
getUptime :: (MonadIO m, Ether.MonadReader' StartTime m) => m Microsecond
getUptime = do
    curTime <- liftIO getCurrentTime
    startTime <- Ether.asks' unStartTime
    let seconds = toRational $ curTime `diffUTCTime` startTime
    pure $ fromMicroseconds $ round $ seconds * 1000 * 1000
