{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Interface to Blocks DB.

module Pos.DB.Block
       ( getBlock
       , getBlockHeader
       , getStoredBlock
       , getUndo

       , deleteBlock
       , putBlock

       , prepareBlockDB

       -- * Load data
       , loadBlundsWhile
       , loadBlundsByDepth
       , loadBlocksWhile
       , loadHeadersWhile
       , loadHeadersByDepth
       ) where

import           Control.Lens        (view, (^.))
import           Data.ByteArray      (convert)
import           Formatting          (sformat, (%))
import           Universum

import           Pos.Binary.Class    (Bi)
import           Pos.Binary.DB       ()
import           Pos.Crypto          (Hash, shortHashF)
import           Pos.DB.Class        (MonadDB, getBlockDB)
import           Pos.DB.Error        (DBError (..))
import           Pos.DB.Functions    (rocksDelete, rocksGetBi, rocksPutBi)
import           Pos.DB.Types        (StoredBlock (..))
import           Pos.Ssc.Class.Types (Ssc)
import           Pos.Types           (Block, BlockHeader, Blund, GenesisBlock,
                                      HasDifficulty (difficultyL), HasPrevBlock,
                                      HeaderHash, Undo (..), genesisHash, headerHash,
                                      prevBlockL)
import qualified Pos.Types           as T
import           Pos.Util            (maybeThrow)

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

-- | Get undo data for block with given hash from Block DB.
getUndo
    :: (MonadDB ssc m)
    => HeaderHash ssc -> m (Maybe Undo)
getUndo = getBi . undoKey

-- | Put given block, its metadata and Undo data into Block DB.
putBlock
    :: (Ssc ssc, MonadDB ssc m)
    => Undo -> Block ssc -> m ()
putBlock undo blk = do
    let h = headerHash blk
    putBi (blockKey h) $ StoredBlock { sbBlock = blk }
    putBi (undoKey h) undo

deleteBlock :: (MonadDB ssc m) => HeaderHash ssc -> m ()
deleteBlock = delete . blockKey

----------------------------------------------------------------------------
-- Load
----------------------------------------------------------------------------

loadDataWhile
    :: forall m a b.
       (Monad m, HasPrevBlock a b)
    => (Hash b -> m a) -> (a -> Bool) -> Hash b -> m [a]
loadDataWhile getter predicate start = doIt start
  where
    doIt :: Hash b -> m [a]
    doIt h
        | h == genesisHash = pure []
        | otherwise = do
            d <- getter h
            let prev = d ^. prevBlockL
            if predicate d
                then (d :) <$> doIt prev
                else pure []

loadDataByDepth
    :: forall m a b.
       (Monad m, HasPrevBlock a b, HasDifficulty a)
    => (Hash b -> m a) -> Word -> Hash b -> m [a]
loadDataByDepth _ 0 _ = pure []
loadDataByDepth getter depth h = do
    -- First of all, we load data corresponding to h.
    top <- getter h
    let topDifficulty = top ^. difficultyL
    -- If top difficulty is 0, we can load all data starting from it.
    -- Then we calculate difficulty of data at which we should stop.
    -- Difficulty of the oldest data to return is 'topDifficulty - depth + 1'
    -- So we are loading all blocks which have difficulty ≥ targetDifficulty.
    let targetDelta = fromIntegral depth - 1
        targetDifficulty
            | topDifficulty <= targetDelta = 0
            | otherwise = topDifficulty - targetDelta
    -- Then we load blocks starting with previous block of already
    -- loaded block.  We load them until we find block with target
    -- difficulty. And then we drop last (oldest) block.
    let prev = top ^. prevBlockL
    (top :) <$>
        loadDataWhile getter ((>= targetDifficulty) . view difficultyL) prev

-- | Load blunds starting from block with header hash equal to given hash
-- and while @predicate@ is true.  The head of returned list is the
-- youngest blund.
loadBlundsWhile
    :: (Ssc ssc, MonadDB ssc m)
    => (Block ssc -> Bool) -> HeaderHash ssc -> m [Blund ssc]
loadBlundsWhile predicate = loadDataWhile getBlundThrow (predicate . fst)

-- | Load blunds which have depth less than given.
loadBlundsByDepth
    :: (Ssc ssc, MonadDB ssc m)
    => Word -> HeaderHash ssc -> m [Blund ssc]
loadBlundsByDepth = loadDataByDepth getBlundThrow

-- | Load blocks starting from block with header hash equal to given hash
-- and while @predicate@ is true.  The head of returned list is the
-- youngest block.
loadBlocksWhile
    :: (Ssc ssc, MonadDB ssc m)
    => (Block ssc -> Bool) -> HeaderHash ssc -> m [Block ssc]
loadBlocksWhile = loadDataWhile getBlockThrow

-- | Load headers starting from block with header hash equal to given hash
-- and while @predicate@ is true.  The head of returned list is the
-- youngest header.
loadHeadersWhile
    :: (Ssc ssc, MonadDB ssc m)
    => (BlockHeader ssc -> Bool) -> HeaderHash ssc -> m [BlockHeader ssc]
loadHeadersWhile = loadDataWhile getHeaderThrow

-- | Load headers which have depth less than given.
loadHeadersByDepth
    :: (Ssc ssc, MonadDB ssc m)
    => Word -> HeaderHash ssc -> m [BlockHeader ssc]
loadHeadersByDepth = loadDataByDepth getHeaderThrow

----------------------------------------------------------------------------
-- Initialization
----------------------------------------------------------------------------

prepareBlockDB
    :: forall ssc m.
       (Ssc ssc, MonadDB ssc m)
    => GenesisBlock ssc -> m ()
prepareBlockDB = putBlock (Undo [] []) . Left

----------------------------------------------------------------------------
-- Keys
----------------------------------------------------------------------------

blockKey :: HeaderHash ssc -> ByteString
blockKey h = "b" <> convert h

undoKey :: HeaderHash ssc -> ByteString
undoKey h = "u" <> convert h

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

----------------------------------------------------------------------------
-- Private functions
----------------------------------------------------------------------------

getBlundThrow
    :: (Ssc ssc, MonadDB ssc m)
    => HeaderHash ssc -> m (Block ssc, Undo)
getBlundThrow hash =
    maybeThrow (DBMalformed $ sformat errFmt hash) =<<
    (liftA2 (,) <$> getBlock hash <*> getUndo hash)
  where
    errFmt = ("getBlockThrow: no blund with HeaderHash: " %shortHashF)

getBlockThrow
    :: (Ssc ssc, MonadDB ssc m)
    => HeaderHash ssc -> m (Block ssc)
getBlockThrow hash = maybeThrow (DBMalformed $ sformat errFmt hash) =<< getBlock hash
  where
    errFmt =
        ("getBlockThrow: no block with HeaderHash: "%shortHashF)

getHeaderThrow
    :: (Ssc ssc, MonadDB ssc m)
    => HeaderHash ssc -> m (BlockHeader ssc)
getHeaderThrow hash = maybeThrow (DBMalformed $ sformat errFmt hash) =<< getBlockHeader hash
  where
    errFmt =
        ("getBlockThrow: no block header with hash: "%shortHashF)
