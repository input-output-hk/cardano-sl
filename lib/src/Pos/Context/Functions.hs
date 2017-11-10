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

import           Control.Lens (views)
import           Data.Time (diffUTCTime, getCurrentTime)
import           Data.Time.Units (Microsecond, fromMicroseconds)

import           Pos.Block.Base (mkGenesisBlock)
import           Pos.Block.BHelpers ()
import           Pos.Context.Context (StartTime (..))
import           Pos.Core (GenesisData (..), HasConfiguration, SlotLeaders, genesisData)
import           Pos.Core.Block (GenesisBlock)
import           Pos.Lrc.Context (lrcActionOnEpoch, lrcActionOnEpochReason, waitLrc)
import           Pos.Lrc.FtsPure (followTheSatoshiUtxo)
import           Pos.Txp.GenesisUtxo (genesisStakes, genesisUtxo)
import           Pos.Txp.Toil (GenesisUtxo (..))
import           Pos.Util.Util (HasLens (lensOf))

----------------------------------------------------------------------------
-- Genesis
----------------------------------------------------------------------------

-- | Compute leaders of the 0-th epoch from stake distribution.
genesisLeaders :: HasConfiguration => SlotLeaders
genesisLeaders = followTheSatoshiUtxo (gdFtsSeed genesisData) utxo
  where
    GenesisUtxo utxo = genesisUtxo

genesisBlock0 :: HasConfiguration => GenesisBlock
genesisBlock0 = mkGenesisBlock Nothing 0 genesisLeaders

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
