{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Higher-level DB functionality.

module Pos.DB.DB
       ( openNodeDBs
       , initNodeDBs
       , getTip
       , getTipBlock
       , getTipBlockHeader
       , loadBlundsFromTipWhile
       , loadBlundsFromTipByDepth
       , sanityCheckDB
       ) where

import qualified Control.Concurrent.ReadWriteLock as RWL
import           Control.Monad.Catch              (MonadMask)
import           System.Directory                 (createDirectoryIfMissing,
                                                   doesDirectoryExist,
                                                   removeDirectoryRecursive)
import           System.FilePath                  ((</>))
import           System.Wlog                      (WithLogger)
import           Universum

import           Pos.Block.Types                  (Blund)
import           Pos.Context.Class                (WithNodeContext)
import           Pos.Context.Functions            (genesisLeadersM)
import           Pos.DB.Block                     (getTipBlock, getTipBlockHeader,
                                                   loadBlundsByDepth, loadBlundsWhile,
                                                   prepareBlockDB)
import           Pos.DB.Class                     (MonadDB, MonadDBCore (..))
import           Pos.DB.Error                     (DBError (DBMalformed))
import           Pos.DB.Functions                 (openDB)
import           Pos.DB.GState.BlockExtra         (prepareGStateBlockExtra)
import           Pos.DB.GState.Common             (getTip)
import           Pos.DB.GState.GState             (prepareGStateDB, sanityCheckGStateDB)
import           Pos.DB.Holder                    (DBHolder)
import           Pos.DB.Misc                      (prepareMiscDB)
import           Pos.DB.Types                     (NodeDBs (..))
import           Pos.Lrc.DB                       (prepareLrcDB)
import           Pos.Ssc.Class.Helpers            (SscHelpersClass)
import           Pos.Types                        (Block, BlockHeader, getBlockHeader,
                                                   headerHash, mkGenesisBlock)
import           Pos.Update.DB                    (getAdoptedBVData)
import           Pos.Util                         (inAssertMode)
import           Pos.Util.Chrono                  (NewestFirst)
#ifdef WITH_EXPLORER
import           Pos.Explorer                     (prepareExplorerDB)
#endif

-- | Open all DBs stored on disk.
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
    _miscLock <- liftIO RWL.new
    pure NodeDBs {..}

-- | Initialize DBs if necessary.
initNodeDBs
    :: forall ssc m.
       ( SscHelpersClass ssc
       , WithNodeContext ssc m
       , MonadDB m )
    => m ()
initNodeDBs = do
    leaders0 <- genesisLeadersM
    let genesisBlock0 = mkGenesisBlock @ssc Nothing 0 leaders0
        initialTip = headerHash genesisBlock0
    prepareBlockDB genesisBlock0
    prepareGStateDB initialTip
    prepareGStateBlockExtra initialTip
    prepareLrcDB
    prepareMiscDB
#ifdef WITH_EXPLORER
    prepareExplorerDB
#endif

-- | Load blunds from BlockDB starting from tip and while the @condition@ is
-- true.
loadBlundsFromTipWhile
    :: (SscHelpersClass ssc, MonadDB m)
    => (Block ssc -> Bool) -> m (NewestFirst [] (Blund ssc))
loadBlundsFromTipWhile condition = getTip >>= loadBlundsWhile condition

-- | Load blunds from BlockDB starting from tip which have depth less than
-- given.
loadBlundsFromTipByDepth
    :: (SscHelpersClass ssc, MonadDB m)
    => Word -> m (NewestFirst [] (Blund ssc))
loadBlundsFromTipByDepth d = getTip >>= loadBlundsByDepth d

sanityCheckDB
    :: (MonadMask m, MonadDB m, WithLogger m)
    => m ()
sanityCheckDB = inAssertMode sanityCheckGStateDB

----------------------------------------------------------------------------
-- Details
----------------------------------------------------------------------------

ensureDirectoryExists
    :: MonadIO m
    => FilePath -> m ()
ensureDirectoryExists = liftIO . createDirectoryIfMissing True

----------------------------------------------------------------------------
-- MonadDB instance
----------------------------------------------------------------------------

instance (MonadIO m, MonadThrow m, MonadCatch m) =>
         MonadDBCore (DBHolder m) where
    dbAdoptedBVData = getAdoptedBVData
