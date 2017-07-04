{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

import           Control.Monad.Catch   (MonadMask)
import           Ether.Internal        (HasLens (..))
import           System.Wlog           (WithLogger)

import           Pos.Block.Core        (Block, BlockHeader, mkGenesisBlock)
import           Pos.Block.Types       (Blund)
import           Pos.Context.Context   (GenesisStakes, GenesisUtxo)
import           Pos.Context.Functions (genesisLeadersM)
import           Pos.Core              (BlockVersionData, Timestamp, headerHash)
import           Pos.DB.Block          (MonadBlockDB, MonadBlockDBWrite,
                                        loadBlundsByDepth, loadBlundsWhile,
                                        prepareBlockDB)
import           Pos.DB.Class          (MonadDB, MonadDBRead (..))
import           Pos.DB.GState.Common  (getTip, getTipBlockGeneric, getTipHeaderGeneric)
import           Pos.DB.GState.GState  (prepareGStateDB, sanityCheckGStateDB)
import           Pos.DB.Misc           (prepareMiscDB)
import           Pos.Lrc.DB            (prepareLrcDB)
import           Pos.Ssc.Class.Helpers (SscHelpersClass)
import           Pos.Update.DB         (getAdoptedBVData)
import           Pos.Util              (inAssertMode)
import           Pos.Util.Chrono       (NewestFirst)
#ifdef WITH_EXPLORER
import           Pos.Explorer.DB       (prepareExplorerDB)
#endif


-- | Initialize DBs if necessary.
initNodeDBs
    :: forall ssc ctx m.
       ( MonadReader ctx m
       , HasLens GenesisUtxo ctx GenesisUtxo
       , HasLens GenesisStakes ctx GenesisStakes
       , MonadBlockDBWrite ssc m
       , SscHelpersClass ssc
       , MonadDB m
       )
    => Timestamp -> m ()
initNodeDBs systemStart = do
    leaders0 <- genesisLeadersM
    let genesisBlock0 = mkGenesisBlock @ssc Nothing 0 leaders0
        initialTip = headerHash genesisBlock0
    prepareBlockDB genesisBlock0
    prepareGStateDB systemStart initialTip
    prepareLrcDB
    prepareMiscDB
#ifdef WITH_EXPLORER
    prepareExplorerDB
#endif


-- | Load blunds from BlockDB starting from tip and while the @condition@ is
-- true.
loadBlundsFromTipWhile
    :: (MonadBlockDB ssc m, MonadDBRead m)
    => (Block ssc -> Bool) -> m (NewestFirst [] (Blund ssc))
loadBlundsFromTipWhile condition = getTip >>= loadBlundsWhile condition

-- | Load blunds from BlockDB starting from tip which have depth less than
-- given.
loadBlundsFromTipByDepth
    :: (MonadBlockDB ssc m, MonadDBRead m)
    => Word -> m (NewestFirst [] (Blund ssc))
loadBlundsFromTipByDepth d = getTip >>= loadBlundsByDepth d

sanityCheckDB
    :: (MonadMask m, WithLogger m, MonadDBRead m)
    => m ()
sanityCheckDB = inAssertMode sanityCheckGStateDB

-- | Specialized version of 'getTipBlockGeneric'.
getTipBlock ::
       forall ssc m. MonadBlockDB ssc m
    => m (Block ssc)
getTipBlock = getTipBlockGeneric @(Block ssc)

-- | Specialized version of 'getTipHeaderGeneric'.
getTipHeader ::
       forall ssc m. MonadBlockDB ssc m
    => m (BlockHeader ssc)
getTipHeader = getTipHeaderGeneric @(Block ssc)

----------------------------------------------------------------------------
-- MonadGState instance
----------------------------------------------------------------------------

gsAdoptedBVDataDefault :: MonadDBRead m => m BlockVersionData
gsAdoptedBVDataDefault = getAdoptedBVData
