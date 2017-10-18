{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Higher-level DB functionality.

module Pos.DB.DB
       ( initNodeDBs

       , getTip
       , getTipBlock
       , getTipHeader
       , loadBlundsFromTipWhile
       , loadBlundsFromTipByDepth

       , sanityCheckDB

       , gsAdoptedBVDataDefault
       ) where

import           Universum

import           Control.Monad.Catch              (MonadMask)
import           System.Wlog                      (WithLogger)

import           Pos.Block.Core                   (Block, BlockHeader)
import           Pos.Block.Types                  (Blund)
import           Pos.Context.Functions            (genesisBlock0)
import           Pos.Core                         (BlockCount, BlockVersionData,
                                                   HasConfiguration, headerHash)
import           Pos.DB.Block                     (MonadBlockDB, MonadBlockDBWrite,
                                                   loadBlundsByDepth, loadBlundsWhile,
                                                   prepareBlockDB)
import           Pos.DB.Class                     (MonadDB, MonadDBRead (..))
import           Pos.DB.GState.Common             (getTip, getTipBlockGeneric,
                                                   getTipHeaderGeneric)
import           Pos.DB.Misc                      (prepareMiscDB)
import           Pos.GState.GState                (prepareGStateDB, sanityCheckGStateDB)
import           Pos.Lrc.DB                       (prepareLrcDB)
import           Pos.Ssc.GodTossing.Configuration (HasGtConfiguration)
import           Pos.Update.DB                    (getAdoptedBVData)
import           Pos.Util                         (inAssertMode)
import           Pos.Util.Chrono                  (NewestFirst)

#ifdef WITH_EXPLORER
import           Pos.Explorer.DB                  (prepareExplorerDB)
#endif


-- | Initialize DBs if necessary.
initNodeDBs
    :: forall ctx m.
       ( MonadReader ctx m
       , MonadBlockDBWrite m
       , MonadDB m
       , HasConfiguration
       , HasGtConfiguration
       )
    => m ()
initNodeDBs = do
    let initialTip = headerHash genesisBlock0
    prepareBlockDB genesisBlock0
    prepareGStateDB initialTip
    prepareLrcDB
    prepareMiscDB
#ifdef WITH_EXPLORER
    prepareExplorerDB
#endif


-- | Load blunds from BlockDB starting from tip and while the @condition@ is
-- true.
loadBlundsFromTipWhile
    :: (MonadBlockDB m)
    => (Block -> Bool) -> m (NewestFirst [] Blund)
loadBlundsFromTipWhile condition = getTip >>= loadBlundsWhile condition

-- | Load blunds from BlockDB starting from tip which have depth less than
-- given.
loadBlundsFromTipByDepth
    :: (MonadBlockDB m)
    => BlockCount -> m (NewestFirst [] Blund)
loadBlundsFromTipByDepth d = getTip >>= loadBlundsByDepth d

sanityCheckDB ::
       ( MonadMask m
       , WithLogger m
       , MonadDBRead m
       , MonadReader ctx m
       )
    => m ()
sanityCheckDB = inAssertMode sanityCheckGStateDB

-- | Specialized version of 'getTipBlockGeneric'.
getTipBlock ::
       (MonadBlockDB m)
    => m Block
getTipBlock = getTipBlockGeneric @Block

-- | Specialized version of 'getTipHeaderGeneric'.
getTipHeader ::
       (MonadBlockDB m)
    => m BlockHeader
getTipHeader = getTipHeaderGeneric @Block

----------------------------------------------------------------------------
-- MonadGState instance
----------------------------------------------------------------------------

gsAdoptedBVDataDefault :: MonadDBRead m => m BlockVersionData
gsAdoptedBVDataDefault = getAdoptedBVData
