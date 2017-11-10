{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Higher-level DB functionality.

module Pos.DB.DB
       ( initNodeDBs
       , sanityCheckDB
       , gsAdoptedBVDataDefault
       ) where

import           Universum

import           Control.Monad.Catch (MonadMask)
import           System.Wlog (WithLogger)

import           Pos.Block.Types (Blund)
import           Pos.Context.Functions (genesisBlock0)
import           Pos.Core (BlockCount, BlockVersionData, HasConfiguration, headerHash)
import           Pos.Core.Block (Block, BlockHeader)
import           Pos.DB.Block (MonadBlockDB, MonadBlockDBWrite, loadBlundsByDepth, loadBlundsWhile,
                               prepareBlockDB)
import           Pos.DB.Class (MonadDB, MonadDBRead (..))
import           Pos.DB.GState.Common (getTip, getTipBlockGeneric, getTipHeaderGeneric)
import           Pos.DB.Misc (prepareMiscDB)
import           Pos.GState.GState (prepareGStateDB, sanityCheckGStateDB)
import           Pos.Lrc.DB (prepareLrcDB)
import           Pos.Ssc.Configuration (HasSscConfiguration)
import           Pos.Update.DB (getAdoptedBVData)
import           Pos.Util (inAssertMode)
import           Pos.Util.Chrono (NewestFirst)

-- | Initialize DBs if necessary.
initNodeDBs
    :: forall ctx m.
       ( MonadReader ctx m
       , MonadBlockDBWrite m
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
    prepareMiscDB

sanityCheckDB ::
       ( MonadMask m
       , WithLogger m
       , MonadDBRead m
       , MonadReader ctx m
       )
    => m ()
sanityCheckDB = inAssertMode sanityCheckGStateDB

----------------------------------------------------------------------------
-- MonadGState instance
----------------------------------------------------------------------------

gsAdoptedBVDataDefault :: MonadDBRead m => m BlockVersionData
gsAdoptedBVDataDefault = getAdoptedBVData
