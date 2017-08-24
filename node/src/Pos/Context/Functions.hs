-- | Functions operating on NodeContext.

module Pos.Context.Functions
       (
         -- * Genesis
         GenesisUtxo(..)
       , genesisUtxoM
       , genesisStakesM
       , genesisLeadersM

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

import           Pos.Context.Context (StartTime (..))
import           Pos.Core            (HasCoreConstants, SlotLeaders, StakesMap)
import           Pos.Genesis         (GenesisUtxo (..), genesisLeaders)
import           Pos.Lrc.Context     (lrcActionOnEpoch, lrcActionOnEpochReason, waitLrc)
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
