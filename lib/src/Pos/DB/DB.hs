{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Higher-level DB functionality.

module Pos.DB.DB
       ( initNodeDBs
       , gsAdoptedBVDataDefault
       ) where

import           Universum

import           Pos.Context.Functions (genesisBlock0)
import           Pos.Core (BlockVersionData, HasConfiguration, headerHash)
import           Pos.DB.Block (prepareBlockDB)
import           Pos.DB.Class (MonadDB, MonadDBRead (..))
import           Pos.GState.GState (prepareGStateDB)
import           Pos.Lrc.DB (prepareLrcDB)
import           Pos.Ssc.Configuration (HasSscConfiguration)
import           Pos.Update.DB (getAdoptedBVData)

-- | Initialize DBs if necessary.
initNodeDBs
    :: forall ctx m.
       ( MonadReader ctx m
       , MonadDB m
       , HasConfiguration
       , HasSscConfiguration
       )
    => m ()
initNodeDBs = do
    let initialTip = headerHash genesisBlock0
    prepareBlockDB genesisBlock0
    prepareGStateDB initialTip
    prepareLrcDB

----------------------------------------------------------------------------
-- MonadGState instance
----------------------------------------------------------------------------

gsAdoptedBVDataDefault :: MonadDBRead m => m BlockVersionData
gsAdoptedBVDataDefault = getAdoptedBVData
