-- | Interface to Blocks DB.

module Pos.Modern.DB.Block
       ( getBlock
       , getBlockHeader
       , getStoredBlock
       , getUndo
       , isBlockInMainChain

       , deleteBlock
       , putBlock
       ) where

import           Data.ByteArray          (convert)
import           Universum

import           Pos.Binary.Class        (Bi)
import           Pos.Binary.Modern.DB    ()
import           Pos.Modern.DB.Class     (MonadDB, getBlockDB)
import           Pos.Modern.DB.Functions (rocksDelete, rocksGetBi, rocksPutBi)
import           Pos.Modern.DB.Types     (StoredBlock (..))
import           Pos.Ssc.Class.Types     (Ssc)
import           Pos.Types               (Block, BlockHeader, HeaderHash, Undo,
                                          headerHash)
import qualified Pos.Types               as T

-- | Get StoredBlock by hash from Block DB.
getStoredBlock
    :: (Ssc ssc, MonadDB ssc m)
    => HeaderHash ssc -> m (Maybe (StoredBlock ssc))
getStoredBlock = getBi . blockKey

-- | Get block with given hash from Block DB.
getBlock
    :: (Ssc ssc, MonadDB ssc m)
    => HeaderHash ssc -> m (Maybe (Block ssc))
getBlock = fmap (fmap sbBlock) . getStoredBlock

-- | Returns header of block that was requested from Block DB.
getBlockHeader
    :: (Ssc ssc, MonadDB ssc m)
    => HeaderHash ssc -> m (Maybe (BlockHeader ssc))
getBlockHeader h = fmap T.getBlockHeader <$> getBlock h

-- | Get block with given hash from Block DB.
isBlockInMainChain
    :: (Ssc ssc, MonadDB ssc m)
    => HeaderHash ssc -> m Bool
isBlockInMainChain = fmap (maybe True sbInMain) . getStoredBlock

-- | Get undo data for block with given hash from Block DB.
getUndo
    :: (MonadDB ssc m)
    => HeaderHash ssc -> m (Maybe Undo)
getUndo = getBi . undoKey

-- | Put given block, its metadata and Undo data into Block DB.
putBlock
    :: (Ssc ssc, MonadDB ssc m)
    => Undo -> Bool -> Block ssc -> m ()
putBlock undo inMainChain blk = do
    let h = headerHash blk
    putBi
        (blockKey h)
        StoredBlock
        { sbBlock = blk
        , sbInMain = inMainChain
        }
    putBi (undoKey h) undo

deleteBlock :: (MonadDB ssc m) => HeaderHash ssc -> m ()
deleteBlock = delete . blockKey

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

getBi
    :: (MonadDB ssc m, Bi v)
    => ByteString -> m (Maybe v)
getBi k = rocksGetBi k =<< getBlockDB

putBi
    :: (MonadDB ssc m, Bi v)
    => ByteString -> v -> m ()
putBi k v = rocksPutBi k v =<< getBlockDB

delete :: (MonadDB ssc m) => ByteString -> m ()
delete k = rocksDelete k =<< getBlockDB

blockKey :: HeaderHash ssc -> ByteString
blockKey h = "b" <> convert h

undoKey :: HeaderHash ssc -> ByteString
undoKey h = "u" <> convert h
