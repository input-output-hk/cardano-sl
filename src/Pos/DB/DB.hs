{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Higher-level DB functionality.

module Pos.DB.DB
       ( openNodeDBs
       , initNodeDBs
       , getTip
       , getTipBlock
       , getTipHeader
       , loadBlundsFromTipWhile
       , loadBlundsFromTipByDepth
       , sanityCheckDB

       , GStateCoreRedirect
       , runGStateCoreRedirect
       ) where

import           Universum

import           Control.Monad.Catch          (MonadMask)
import           Control.Monad.Trans.Identity (IdentityT (..))
import           Data.Coerce                  (coerce)
import qualified Ether
import           Formatting                   (sformat, stext, (%))
import           System.Directory             (createDirectoryIfMissing,
                                               doesDirectoryExist,
                                               removeDirectoryRecursive)
import           System.FilePath              ((</>))
import           System.Wlog                  (WithLogger)

import           Pos.Block.Core               (Block, BlockHeader, mkGenesisBlock)
import           Pos.Block.Types              (Blund)
import           Pos.Context.Context          (GenesisLeaders, GenesisUtxo, NodeParams)
import           Pos.Context.Functions        (genesisLeadersM)
import           Pos.Core                     (HeaderHash, headerHash)
import           Pos.DB.Block                 (MonadBlockDB, blkGetBlock, blkGetHeader,
                                               loadBlundsByDepth, loadBlundsWhile,
                                               prepareBlockDB)
import           Pos.DB.Class                 (MonadDB, MonadDBPure (..),
                                               MonadGStateCore (..))
import           Pos.DB.Error                 (DBError (DBMalformed))
import           Pos.DB.Functions             (openDB)
import           Pos.DB.GState.BlockExtra     (prepareGStateBlockExtra)
import           Pos.DB.GState.Common         (getTip)
import           Pos.DB.GState.GState         (prepareGStateDB, sanityCheckGStateDB)
import           Pos.DB.Misc                  (prepareMiscDB)
import           Pos.DB.Types                 (NodeDBs (..))
import           Pos.Lrc.DB                   (prepareLrcDB)
import           Pos.Ssc.Class.Helpers        (SscHelpersClass)
import           Pos.Update.DB                (getAdoptedBVData)
import           Pos.Util                     (inAssertMode)
import           Pos.Util.Chrono              (NewestFirst)
import qualified Pos.Util.Concurrent.RWLock   as RWL
#ifdef WITH_EXPLORER
import           Pos.Explorer.DB              (prepareExplorerDB)
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
    _miscLock <- RWL.new
    pure NodeDBs {..}

-- | Initialize DBs if necessary.
initNodeDBs
    :: forall ssc m.
       ( SscHelpersClass ssc
       , Ether.MonadReader' GenesisUtxo m
       , Ether.MonadReader' GenesisLeaders m
       , Ether.MonadReader' NodeParams m
       , MonadDB m
       , MonadDBPure m
       )
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

-- | Get 'Block' corresponding to tip.
getTipBlock
    :: forall ssc m. (MonadBlockDB ssc m)
    => m (Block ssc)
getTipBlock = getTipSomething @ssc "block" blkGetBlock

-- | Get 'BlockHeader' corresponding to tip.
getTipHeader
    :: forall ssc m. (SscHelpersClass ssc, MonadDBPure m)
    => m (BlockHeader ssc)
getTipHeader = getTipSomething @ssc "header" blkGetHeader

getTipSomething
    :: (SscHelpersClass ssc, MonadDBPure m)
    => Text -> (HeaderHash -> m (Maybe smth)) -> m smth
getTipSomething smthDescription smthGetter =
    maybe onFailure pure =<< smthGetter =<< getTip
  where
    fmt = "there is no "%stext%" corresponding to tip"
    onFailure = throwM $ DBMalformed $ sformat fmt smthDescription

-- | Load blunds from BlockDB starting from tip and while the @condition@ is
-- true.
loadBlundsFromTipWhile
    :: (MonadBlockDB ssc m, MonadDBPure m)
    => (Block ssc -> Bool) -> m (NewestFirst [] (Blund ssc))
loadBlundsFromTipWhile condition = getTip >>= loadBlundsWhile condition

-- | Load blunds from BlockDB starting from tip which have depth less than
-- given.
loadBlundsFromTipByDepth
    :: (MonadBlockDB ssc m, MonadDBPure m)
    => Word -> m (NewestFirst [] (Blund ssc))
loadBlundsFromTipByDepth d = getTip >>= loadBlundsByDepth d

sanityCheckDB
    :: (MonadMask m, MonadDB m, WithLogger m, MonadDBPure m)
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
-- MonadGStateCore instance
----------------------------------------------------------------------------

data GStateCoreRedirectTag

type GStateCoreRedirect =
    Ether.TaggedTrans GStateCoreRedirectTag IdentityT

runGStateCoreRedirect :: GStateCoreRedirect m a -> m a
runGStateCoreRedirect = coerce

instance
    (MonadDBPure m, t ~ IdentityT) =>
        MonadGStateCore (Ether.TaggedTrans GStateCoreRedirectTag t m)
  where
    gsAdoptedBVData = getAdoptedBVData
