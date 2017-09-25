-- | Functions operating on NodeContext.

module Pos.Context.Functions
       (
         -- * Genesis
         GenesisUtxo(..)
       , genesisUtxo
       , genesisStakes
       , genesisLeaders
       , genesisBlock0

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
import           Pos.Core            (GenesisData (..), HasConfiguration, SlotLeaders,
                                      genesisData)
import           Pos.Lrc.Context     (lrcActionOnEpoch, lrcActionOnEpochReason, waitLrc)
import           Pos.Lrc.FtsPure     (followTheSatoshiUtxo)
import           Pos.Ssc.Class       (SscHelpersClass)
import           Pos.Txp.GenesisUtxo (genesisStakes, genesisUtxo)
import           Pos.Txp.Toil        (GenesisUtxo (..))
import           Pos.Util.Util       (HasLens (lensOf))

----------------------------------------------------------------------------
-- Genesis
----------------------------------------------------------------------------

-- | Compute leaders of the 0-th epoch from stake distribution.
genesisLeaders :: HasConfiguration => SlotLeaders
genesisLeaders = followTheSatoshiUtxo
                    (gdBootStakeholders genesisData)
                    (gdFtsSeed genesisData)
                    utxo
  where
    GenesisUtxo utxo = genesisUtxo

genesisBlock0 :: forall ssc . (HasConfiguration, SscHelpersClass ssc)
    => GenesisBlock ssc
genesisBlock0 = mkGenesisBlock @ssc Nothing 0 genesisLeaders

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
