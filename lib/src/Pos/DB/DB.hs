{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Higher-level DB functionality.

module Pos.DB.DB
       ( initNodeDBs
       , gsAdoptedBVDataDefault
       ) where

import           Universum

import           Pos.Chain.Block (genesisBlock0, headerHash)
import           Pos.Chain.Lrc (genesisLeaders)
import           Pos.Core (GenesisHash (..), SlotCount, genesisHash)
import           Pos.Core.Update (BlockVersionData)
import           Pos.Crypto (ProtocolMagic)
import           Pos.DB.Block (prepareBlockDB)
import           Pos.DB.Class (MonadDB, MonadDBRead (..))
import           Pos.DB.Lrc (prepareLrcDB)
import           Pos.DB.Update (getAdoptedBVData)
import           Pos.GState.GState (prepareGStateDB)

-- | Initialize DBs if necessary.
initNodeDBs
    :: forall ctx m.
       ( MonadReader ctx m
       , MonadDB m
       )
    => ProtocolMagic -> SlotCount -> m ()
initNodeDBs pm epochSlots = do
    let initialTip = headerHash gb
    prepareBlockDB gb
    prepareGStateDB initialTip
    prepareLrcDB epochSlots
  where
    gb = genesisBlock0 pm (GenesisHash genesisHash) (genesisLeaders epochSlots)

----------------------------------------------------------------------------
-- MonadGState instance
----------------------------------------------------------------------------

gsAdoptedBVDataDefault :: MonadDBRead m => m BlockVersionData
gsAdoptedBVDataDefault = getAdoptedBVData
