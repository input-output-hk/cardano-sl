-- | Higher-level DB functionality.

module Pos.DB.DB
       ( initNodeDBs
       , gsAdoptedBVDataDefault
       ) where

import           Universum

import           Pos.Core (BlockVersionData, GenesisHash (..),
                     ProtocolConstants, genesisHash, headerHash, pcEpochSlots)
import           Pos.Core.Block.Constructors (genesisBlock0)
import           Pos.Crypto (ProtocolMagic)
import           Pos.DB.Block (prepareBlockDB)
import           Pos.DB.Class (MonadDB, MonadDBRead (..))
import           Pos.GState.GState (prepareGStateDB)
import           Pos.Lrc.DB (prepareLrcDB)
import           Pos.Lrc.Genesis (genesisLeaders)
import           Pos.Update.DB (getAdoptedBVData)

-- | Initialize DBs if necessary.
initNodeDBs
    :: forall ctx m
     . (MonadReader ctx m, MonadDB m)
    => ProtocolMagic
    -> ProtocolConstants
    -> m ()
initNodeDBs pm pc = do
    let initialTip = headerHash gb
    prepareBlockDB gb
    prepareGStateDB pc initialTip
    prepareLrcDB epochSlots
  where
    epochSlots = pcEpochSlots pc
    gb = genesisBlock0 pm (GenesisHash genesisHash) (genesisLeaders epochSlots)

----------------------------------------------------------------------------
-- MonadGState instance
----------------------------------------------------------------------------

gsAdoptedBVDataDefault :: MonadDBRead m => m BlockVersionData
gsAdoptedBVDataDefault = getAdoptedBVData
