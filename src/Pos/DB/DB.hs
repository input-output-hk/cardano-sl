{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

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
       , DbCoreRedirect
       , runDbCoreRedirect
       ) where

import           Universum

import qualified Control.Concurrent.ReadWriteLock as RWL
import           Control.Monad.Catch              (MonadMask)
import           Data.Coerce                      (coerce)
import qualified Ether
import           System.Directory                 (createDirectoryIfMissing,
                                                   doesDirectoryExist,
                                                   removeDirectoryRecursive)
import           System.FilePath                  ((</>))
import           System.Wlog                      (WithLogger)

import           Pos.Block.Pure                   (mkGenesisBlock)
import           Pos.Block.Types                  (Blund)
import           Pos.Context.Class                (WithNodeContext)
import           Pos.Context.Context              (GenesisLeaders, GenesisUtxo,
                                                   NodeParams)
import           Pos.Context.Functions            (genesisLeadersM)
import           Pos.DB.Block                     (getBlock, loadBlundsByDepth,
                                                   loadBlundsWhile, prepareBlockDB)
import           Pos.DB.Class                     (MonadDB, MonadDBCore (..))
import           Pos.DB.Error                     (DBError (DBMalformed))
import           Pos.DB.Functions                 (openDB)
import           Pos.DB.GState.BlockExtra         (prepareGStateBlockExtra)
import           Pos.DB.GState.Common             (getTip)
import           Pos.DB.GState.GState             (prepareGStateDB, sanityCheckGStateDB)
import           Pos.DB.Misc                      (prepareMiscDB)
import           Pos.DB.Types                     (NodeDBs (..))
import           Pos.Lrc.DB                       (prepareLrcDB)
import           Pos.Ssc.Class.Helpers            (SscHelpersClass)
import           Pos.Types                        (Block, BlockHeader, getBlockHeader,
                                                   headerHash)
import           Pos.Update.DB                    (getAdoptedBVData)
import           Pos.Util                         (inAssertMode)
import           Pos.Util.Chrono                  (NewestFirst)

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
       , Ether.MonadReader' GenesisUtxo m
       , Ether.MonadReader' GenesisLeaders m
       , Ether.MonadReader' NodeParams m
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

-- | Get block corresponding to tip.
getTipBlock
    :: (SscHelpersClass ssc, MonadDB m)
    => m (Block ssc)
getTipBlock = maybe onFailure pure =<< getBlock =<< getTip
  where
    onFailure = throwM $ DBMalformed "there is no block corresponding to tip"

-- | Get BlockHeader corresponding to tip.
getTipBlockHeader
    :: (SscHelpersClass ssc, MonadDB m)
    => m (BlockHeader ssc)
getTipBlockHeader = getBlockHeader <$> getTipBlock

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
-- MonadDBCore instance
----------------------------------------------------------------------------

data DbCoreRedirectTag

type DbCoreRedirect =
    Ether.TaggedTrans DbCoreRedirectTag Ether.IdentityT

runDbCoreRedirect :: DbCoreRedirect m a -> m a
runDbCoreRedirect = coerce

instance
    (MonadDB m, t ~ Ether.IdentityT) =>
        MonadDBCore (Ether.TaggedTrans DbCoreRedirectTag t m)
  where
    dbAdoptedBVData = getAdoptedBVData
