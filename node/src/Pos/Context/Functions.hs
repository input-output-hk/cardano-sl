-- | Functions operating on NodeContext.

module Pos.Context.Functions
       (
         -- * Genesis
         GenesisUtxo(..)
       , genesisUtxoM
       , genesisStakesM
       , genesisLeadersM
       , genesisBlock0M

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

import           Pos.Block.Core      (GenesisBlock, mkGenesisBlock)
import           Pos.Context.Context (StartTime (..))
import           Pos.Core            (GenesisWStakeholders, HasCoreConstants, SlotLeaders,
                                      StakesMap)
import           Pos.Genesis         (GenesisContext (..), GenesisUtxo (..),
                                      genesisLeaders)
import           Pos.Lrc.Context     (lrcActionOnEpoch, lrcActionOnEpochReason, waitLrc)
import           Pos.Ssc.Class       (SscHelpersClass)
import           Pos.Txp.Toil        (utxoToStakes)
import           Pos.Util.Util       (HasLens (lensOf), HasLens', lensOf')

----------------------------------------------------------------------------
-- Genesis
----------------------------------------------------------------------------

genesisUtxoM ::
       (Functor m, MonadReader ctx m, HasLens GenesisUtxo ctx GenesisUtxo)
    => m GenesisUtxo
genesisUtxoM = view (lensOf @GenesisUtxo)

genesisStakesM ::
       ( Functor m
       , MonadReader ctx m
       , HasLens' ctx GenesisUtxo
       , HasLens' ctx GenesisWStakeholders
       )
    => m StakesMap
genesisStakesM = do
    gws <- view (lensOf @GenesisWStakeholders)
    views (lensOf @GenesisUtxo) $ utxoToStakes gws . unGenesisUtxo

genesisLeadersM ::
       ( Functor m
       , MonadReader ctx m
       , HasLens' ctx GenesisUtxo
       , HasLens' ctx GenesisWStakeholders
       , HasCoreConstants
       )
    => m SlotLeaders
genesisLeadersM =
    genesisLeaders <$> (GenesisContext <$> genesisUtxoM <*> view lensOf')

genesisBlock0M ::
    forall ssc ctx m. ( Functor m, MonadReader ctx m
                      , HasLens GenesisUtxo ctx GenesisUtxo
                      , HasLens' ctx GenesisWStakeholders
                      , HasCoreConstants, SscHelpersClass ssc)
    => m (GenesisBlock ssc)
genesisBlock0M = mkGenesisBlock @ssc Nothing 0 <$> genesisLeadersM

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
