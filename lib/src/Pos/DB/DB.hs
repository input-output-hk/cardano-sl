-- | Higher-level DB functionality.

module Pos.DB.DB
       ( initNodeDBs
       , gsAdoptedBVDataDefault
       ) where

import           Universum

import           Pos.Chain.Block (genesisBlock0, headerHash)
import           Pos.Chain.Lrc (genesisLeaders)
import           Pos.Core as Core (Config (..), GenesisHash (..),
                     configEpochSlots, genesisHash)
import           Pos.Core.Update (BlockVersionData)
import           Pos.DB.Block (prepareBlockDB)
import           Pos.DB.Class (MonadDB, MonadDBRead (..))
import           Pos.DB.Lrc (prepareLrcDB)
import           Pos.DB.Update (getAdoptedBVData)
import           Pos.GState.GState (prepareGStateDB)

-- | Initialize DBs if necessary.
initNodeDBs
    :: forall ctx m
     . (MonadReader ctx m, MonadDB m)
    => Core.Config
    -> m ()
initNodeDBs coreConfig = do
    let initialTip = headerHash gb
    prepareBlockDB gb
    prepareGStateDB (configProtocolConstants coreConfig) initialTip
    prepareLrcDB epochSlots
  where
    epochSlots = configEpochSlots coreConfig
    gb         = genesisBlock0 (configProtocolMagic coreConfig)
                               (GenesisHash genesisHash)
                               (genesisLeaders epochSlots)

----------------------------------------------------------------------------
-- MonadGState instance
----------------------------------------------------------------------------

gsAdoptedBVDataDefault :: MonadDBRead m => m BlockVersionData
gsAdoptedBVDataDefault = getAdoptedBVData
