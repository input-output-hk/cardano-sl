-- | Interface to Blocks DB.

module Pos.Modern.DB.Block
       ( getBlock
       , getStoredBlock
       , getUndo
       , isBlockInMainChain
       ) where

import           Universum

import           Pos.Binary              (Bi, encodeStrict)
import           Pos.Modern.DB.Class     (MonadDB, getBlockDB)
import           Pos.Modern.DB.Functions (rocksGetBi)
import           Pos.Modern.DB.Types     (StoredBlock (..))
import           Pos.Ssc.Class           (Ssc)
import           Pos.Types               (Block, HeaderHash, Undo)

getBi
    :: (MonadDB ssc m, Bi v)
    => ByteString -> m (Maybe v)
getBi k = rocksGetBi k =<< getBlockDB

-- | Get StoredBlock by hash from Block DB.
getStoredBlock
    :: (Ssc ssc, MonadDB ssc m)
    => HeaderHash ssc -> m (Maybe (StoredBlock ssc))
getStoredBlock = getBi . ("b" <>) . encodeStrict

-- | Get block with given hash from Block DB.
getBlock
    :: (Ssc ssc, MonadDB ssc m)
    => HeaderHash ssc -> m (Maybe (Block ssc))
getBlock = fmap (fmap sbBlock) . getStoredBlock

-- | Get block with given hash from Block DB.
isBlockInMainChain
    :: (Ssc ssc, MonadDB ssc m)
    => HeaderHash ssc -> m Bool
isBlockInMainChain = fmap (maybe True sbInMain) . getStoredBlock

-- | Get undo data for block with given hash from Block DB.
getUndo
    :: (MonadDB ssc m)
    => HeaderHash ssc -> m (Maybe Undo)
getUndo = getBi . ("u" <>) . encodeStrict
