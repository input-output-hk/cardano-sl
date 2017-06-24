{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Higher-level DB functionality.

module Pos.DB.DB
       ( openNodeDBs
       , initNodeDBs
       , closeNodeDBs
       , getTip
       , getTipBlock
       , getTipHeader
       , loadBlundsFromTipWhile
       , loadBlundsFromTipByDepth
       , sanityCheckDB
       , gsAdoptedBVDataDefault
       ) where

import           Universum

import           Control.Monad.Catch        (MonadMask)
import qualified Ether
import           System.Directory           (createDirectoryIfMissing, doesDirectoryExist,
                                             removeDirectoryRecursive)
import           System.FilePath            ((</>))
import           System.Wlog                (WithLogger)

import           Pos.Block.Core             (Block, BlockHeader, mkGenesisBlock)
import           Pos.Block.Types            (Blund)
import           Pos.Context.Context        (GenesisUtxo)
import           Pos.Context.Functions      (genesisLeadersM)
import           Pos.Core                   (BlockVersionData, Timestamp, headerHash)
import           Pos.DB.Block               (MonadBlockDB, MonadBlockDBWrite,
                                             loadBlundsByDepth, loadBlundsWhile,
                                             prepareBlockDB)
import           Pos.DB.Class               (MonadDB, MonadDBRead (..))
import           Pos.DB.Functions           (closeDB, openDB)
import           Pos.DB.GState.Common       (getTip, getTipBlockGeneric,
                                             getTipHeaderGeneric)
import           Pos.DB.GState.GState       (prepareGStateDB, sanityCheckGStateDB)
import           Pos.DB.Misc                (prepareMiscDB)
import           Pos.DB.Types               (NodeDBs (..))
import           Pos.Lrc.DB                 (prepareLrcDB)
import           Pos.Update.DB              (getAdoptedBVData)
import           Pos.Util                   (inAssertMode)
import           Pos.Util.Chrono            (NewestFirst)
import qualified Pos.Util.Concurrent.RWLock as RWL
#ifdef WITH_EXPLORER
import           Pos.Explorer.DB            (prepareExplorerDB)
#endif

-- | Open all DBs stored on disk.
-- Don't forget to use 'closeNodeDBs' eventually.
openNodeDBs
    :: (MonadIO m)
    => Bool -> FilePath -> m NodeDBs
openNodeDBs recreate fp = do
    liftIO $
        whenM ((recreate &&) <$> doesDirectoryExist fp) $
            removeDirectoryRecursive fp
    let blocksDir = fp </> "blocks"
    let blocksIndexPath = blocksDir </> "index"
    let _blockDataDir = blocksDir </> "data"
    let gStatePath = fp </> "gState"
    let lrcPath = fp </> "lrc"
    let miscPath = fp </> "misc"
    mapM_ ensureDirectoryExists [ blocksDir
                                , _blockDataDir
                                , blocksIndexPath
                                , gStatePath
                                , lrcPath
                                , miscPath]
    _blockIndexDB <- openDB blocksIndexPath
    _gStateDB <- openDB gStatePath
    _lrcDB <- openDB lrcPath
    _miscDB <- openDB miscPath
    _miscLock <- RWL.new
    pure NodeDBs {..}

-- | Initialize DBs if necessary.
initNodeDBs
    :: forall ssc m.
       ( Ether.MonadReader' GenesisUtxo m
       , MonadBlockDBWrite ssc m
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

-- | Safely close all databases from 'NodeDBs'.
closeNodeDBs :: MonadIO m => NodeDBs -> m ()
closeNodeDBs NodeDBs {..} =
    mapM_ closeDB [_blockIndexDB, _gStateDB, _lrcDB, _miscDB]

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
-- Details
----------------------------------------------------------------------------

ensureDirectoryExists
    :: MonadIO m
    => FilePath -> m ()
ensureDirectoryExists = liftIO . createDirectoryIfMissing True

----------------------------------------------------------------------------
-- MonadGState instance
----------------------------------------------------------------------------

gsAdoptedBVDataDefault :: MonadDBRead m => m BlockVersionData
gsAdoptedBVDataDefault = getAdoptedBVData
