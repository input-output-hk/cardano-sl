-- | Higher-level DB functionality.

module Pos.DB.DB
       ( initNodeDBs
       , gsAdoptedBVDataDefault
       ) where

import           Universum

import           Pos.Chain.Block (genesisBlock0, headerHash)
import           Pos.Chain.Genesis as Genesis (Config (..))
import           Pos.Chain.Lrc (genesisLeaders)
import           Pos.Chain.Update (BlockVersionData)
import           Pos.DB.Block (prepareBlockDB)
import           Pos.DB.Class (MonadDB, MonadDBRead (..))
import           Pos.DB.Lrc (prepareLrcDB)
import           Pos.DB.Update (getAdoptedBVData)
import           Pos.GState.GState (prepareGStateDB)

-- | Initialize DBs if necessary.
initNodeDBs
    :: forall ctx m
     . (MonadReader ctx m, MonadDB m)
    => Genesis.Config
    -> m ()
initNodeDBs genesisConfig = do
    let initialTip = headerHash gb
    prepareBlockDB gb
    prepareGStateDB genesisConfig initialTip
    prepareLrcDB genesisConfig
  where
    gb = genesisBlock0 (configProtocolMagic genesisConfig)
                       (configGenesisHash genesisConfig)
                       (genesisLeaders genesisConfig)

----------------------------------------------------------------------------
-- MonadGState instance
----------------------------------------------------------------------------

gsAdoptedBVDataDefault :: MonadDBRead m => m BlockVersionData
gsAdoptedBVDataDefault = getAdoptedBVData
