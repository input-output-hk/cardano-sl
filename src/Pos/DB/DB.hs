{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Higher-level DB functionality.

module Pos.DB.DB
       ( openNodeDBs
       , initNodeDBs
       , getTip
       , getTipBlock
       , getTipBlockHeader
       , loadBlocksFromTipWhile
       ) where

import           Control.Monad.Trans.Resource (MonadResource)
import           System.Directory             (createDirectoryIfMissing,
                                               doesDirectoryExist,
                                               removeDirectoryRecursive)
import           System.FilePath              ((</>))
import           Universum

import           Pos.Context.Class            (WithNodeContext)
import           Pos.Context.Functions        (genesisLeadersM)
import           Pos.DB.Block                 (getBlock, loadBlocksWithUndoWhile,
                                               prepareBlockDB)
import           Pos.DB.Class                 (MonadDB)
import           Pos.DB.Error                 (DBError (DBMalformed))
import           Pos.DB.Functions             (openDB)
import           Pos.DB.GState.Common         (getTip)
import           Pos.DB.GState.GState         (prepareGStateDB)
import           Pos.DB.Lrc                   (prepareLrcDB)
import           Pos.DB.Misc                  (prepareMiscDB)
import           Pos.DB.Types                 (NodeDBs (..))
import           Pos.Ssc.Class.Types          (Ssc)
import           Pos.Types                    (Block, BlockHeader, Undo, getBlockHeader,
                                               headerHash, mkGenesisBlock)

-- | Open all DBs stored on disk.
openNodeDBs
    :: (Ssc ssc, MonadIO m, MonadThrow m)
    => Bool -> FilePath -> m (NodeDBs ssc)
openNodeDBs recreate fp = do
    liftIO $
        whenM ((recreate &&) <$> doesDirectoryExist fp) $
            removeDirectoryRecursive fp
    let blockPath = fp </> "blocks"
    let gStatePath = fp </> "gState"
    let lrcPath = fp </> "lrc"
    let miscPath = fp </> "misc"
    mapM_ ensureDirectoryExists [blockPath, gStatePath, lrcPath, miscPath]
    _blockDB <- openDB blockPath
    _gStateDB <- openDB gStatePath
    _lrcDB <- openDB lrcPath
    _miscDB <- openDB miscPath
    return NodeDBs {..}

-- | Initialize DBs if necessary.
initNodeDBs
    :: forall ssc m.
       (Ssc ssc, WithNodeContext ssc m, MonadDB ssc m)
    => m ()
initNodeDBs = do
    leaders0 <- genesisLeadersM
    let genesisBlock0 = mkGenesisBlock Nothing 0 leaders0
        initialTip = headerHash genesisBlock0
    prepareBlockDB genesisBlock0
    prepareGStateDB initialTip
    prepareLrcDB
    prepareMiscDB

-- | Get block corresponding to tip.
getTipBlock
    :: (Ssc ssc, MonadDB ssc m)
    => m (Block ssc)
getTipBlock = maybe onFailure pure =<< getBlock =<< getTip
  where
    onFailure = throwM $ DBMalformed "there is no block corresponding to tip"

-- | Get BlockHeader corresponding to tip.
getTipBlockHeader
    :: (Ssc ssc, MonadDB ssc m)
    => m (BlockHeader ssc)
getTipBlockHeader = getBlockHeader <$> getTipBlock

-- | Load blocks from BlockDB starting from tip and while @condition@ is true.
-- The head of returned list is the youngest block.
loadBlocksFromTipWhile
    :: (Ssc ssc, MonadDB ssc m)
    => (Block ssc -> Int -> Bool) -> m [(Block ssc, Undo)]
loadBlocksFromTipWhile condition = getTip >>= loadBlocksWithUndoWhile condition

----------------------------------------------------------------------------
-- Details
----------------------------------------------------------------------------

ensureDirectoryExists
    :: MonadIO m
    => FilePath -> m ()
ensureDirectoryExists = liftIO . createDirectoryIfMissing True
