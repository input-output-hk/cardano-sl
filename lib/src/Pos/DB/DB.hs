{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Higher-level DB functionality.

module Pos.DB.DB
       ( initNodeDBs
       , gsAdoptedBVDataDefault
       ) where

import           Universum

import           Pos.Block.Base
    (genesisBlock0)
import           Pos.Core
    (BlockVersionData, GenesisHash (..), genesisHash, headerHash,
    protocolMagic)
import           Pos.DB.Block
    (prepareBlockDB)
import           Pos.DB.Class
    (MonadDB, MonadDBRead (..))
import           Pos.GState.GState
    (prepareGStateDB)
import           Pos.Lrc.DB
    (prepareLrcDB)
import           Pos.Lrc.Genesis
    (genesisLeaders)
import           Pos.Update.DB
    (getAdoptedBVData)

-- | Initialize DBs if necessary.
initNodeDBs
    :: forall ctx m.
       ( MonadReader ctx m
       , MonadDB m
       )
    => m ()
initNodeDBs = do
    let initialTip = headerHash gb
    prepareBlockDB gb
    prepareGStateDB initialTip
    prepareLrcDB
  where
    gb = genesisBlock0 protocolMagic (GenesisHash genesisHash) genesisLeaders

----------------------------------------------------------------------------
-- MonadGState instance
----------------------------------------------------------------------------

gsAdoptedBVDataDefault :: MonadDBRead m => m BlockVersionData
gsAdoptedBVDataDefault = getAdoptedBVData
