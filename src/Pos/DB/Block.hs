{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Interface to Blocks DB.

module Pos.DB.Block
       ( getBlock
       , getBlockHeader
       , getStoredBlock
       , getUndo
       , getBlockWithUndo

       , deleteBlock
       , putBlock

       , prepareBlockDB

       -- * Load data
       , loadBlundsWhile
       , loadBlundsByDepth
       , loadBlocksWhile
       , loadHeadersWhile
       , loadHeadersByDepth
       , loadHeadersByDepthWhile
       ) where

import           Control.Lens              ((^.))
import           Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import           Data.ByteArray            (convert)
import           Data.Default              (Default (def))
import           Formatting                (sformat, (%))
import           Universum

import           Pos.Binary.Block          ()
import           Pos.Binary.Class          (Bi)
import           Pos.Binary.DB             ()
import           Pos.Block.Types           (Blund, Undo (..))
import           Pos.Crypto                (shortHashF)
import           Pos.DB.Class              (MonadDB, getBlockDB)
import           Pos.DB.Error              (DBError (..))
import           Pos.DB.Functions          (rocksDelete, rocksGetBi, rocksPutBi)
import           Pos.DB.Types              (StoredBlock (..))
import           Pos.Ssc.Class.Types       (Ssc)
import           Pos.Types                 (Block, BlockHeader, GenesisBlock,
                                            HasDifficulty (difficultyL), HasPrevBlock,
                                            HeaderHash, genesisHash, headerHash,
                                            prevBlockL)
import qualified Pos.Types                 as T
import           Pos.Util                  (maybeThrow)

-- | Get StoredBlock by hash from Block DB.
getStoredBlock
    :: (Ssc ssc, MonadDB ssc m)
    => HeaderHash -> m (Maybe (StoredBlock ssc))
getStoredBlock = getBi . blockKey

-- | Get block with given hash from Block DB.
getBlock
    :: (Ssc ssc, MonadDB ssc m)
    => HeaderHash -> m (Maybe (Block ssc))
getBlock = fmap (fmap sbBlock) . getStoredBlock

-- | Returns header of block that was requested from Block DB.
getBlockHeader
    :: (Ssc ssc, MonadDB ssc m)
    => HeaderHash -> m (Maybe (BlockHeader ssc))
getBlockHeader h = fmap T.getBlockHeader <$> getBlock h

-- | Get undo data for block with given hash from Block DB.
getUndo :: MonadDB ssc m => HeaderHash -> m (Maybe Undo)
getUndo = getBi . undoKey

-- | Retrieves block and undo together.
getBlockWithUndo
    :: (Ssc ssc, MonadDB ssc m)
    => HeaderHash -> m (Maybe (Block ssc, Undo))
getBlockWithUndo x =
    runMaybeT $ (,) <$> MaybeT (getBlock x) <*> MaybeT (getUndo x)

-- | Put given block, its metadata and Undo data into Block DB.
putBlock
    :: (Ssc ssc, MonadDB ssc m)
    => Undo -> Block ssc -> m ()
putBlock undo blk = do
    let h = headerHash blk
    putBi (blockKey h) $ StoredBlock { sbBlock = blk }
    putBi (undoKey h) undo

deleteBlock :: (MonadDB ssc m) => HeaderHash -> m ()
deleteBlock = delete . blockKey

----------------------------------------------------------------------------
-- Load
----------------------------------------------------------------------------

loadDataWhile
    :: forall m a .
       (Monad m, HasPrevBlock a)
    => (HeaderHash -> m a) -> (a -> Bool) -> HeaderHash -> m [a]
loadDataWhile getter predicate start = doIt start
  where
    doIt :: HeaderHash -> m [a]
    doIt h
        | h == genesisHash = pure []
        | otherwise = do
            d <- getter h
            let prev = d ^. prevBlockL
            if predicate d
                then (d :) <$> doIt prev
                else pure []

-- For depth 'd' load blocks that have depth < 'd'. Given header
-- (newest one) is assumed to have depth 0.
loadDataByDepth
    :: forall m a .
       (Monad m, HasPrevBlock a, HasDifficulty a)
    => (HeaderHash -> m a) -> (a -> Bool) -> Word -> HeaderHash -> m [a]
loadDataByDepth _ _ 0 _ = pure []
loadDataByDepth getter extraPredicate depth h = do
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
        loadDataWhile
        getter
        (\a -> a ^. difficultyL >= targetDifficulty && extraPredicate a)
        prev

-- | Load blunds starting from block with header hash equal to given hash
-- and while @predicate@ is true.  The head of returned list is the
-- youngest blund.
loadBlundsWhile
    :: (Ssc ssc, MonadDB ssc m)
    => (Block ssc -> Bool) -> HeaderHash -> m [Blund ssc]
loadBlundsWhile predicate = loadDataWhile getBlundThrow (predicate . fst)

-- | Load blunds which have depth less than given.
loadBlundsByDepth
    :: (Ssc ssc, MonadDB ssc m)
    => Word -> HeaderHash -> m [Blund ssc]
loadBlundsByDepth = loadDataByDepth getBlundThrow (const True)

-- | Load blocks starting from block with header hash equal to given
-- hash and while @predicate@ is true. The head of returned list is
-- the youngest block.
loadBlocksWhile
    :: (Ssc ssc, MonadDB ssc m)
    => (Block ssc -> Bool) -> HeaderHash -> m [Block ssc]
loadBlocksWhile = loadDataWhile getBlockThrow

-- | Load headers starting from block with header hash equal to given
-- hash and while @predicate@ is true. The head of returned list is
-- the youngest header.
loadHeadersWhile
    :: (Ssc ssc, MonadDB ssc m)
    => (BlockHeader ssc -> Bool) -> HeaderHash -> m [BlockHeader ssc]
loadHeadersWhile = loadDataWhile getHeaderThrow

-- | Load headers which have depth less than given.
loadHeadersByDepth
    :: (Ssc ssc, MonadDB ssc m)
    => Word -> HeaderHash -> m [BlockHeader ssc]
loadHeadersByDepth = loadDataByDepth getHeaderThrow (const True)

-- | Load headers which have depth less than given and match some
-- criterion.
loadHeadersByDepthWhile
    :: (Ssc ssc, MonadDB ssc m)
    => (BlockHeader ssc -> Bool) -> Word -> HeaderHash -> m [BlockHeader ssc]
loadHeadersByDepthWhile = loadDataByDepth getHeaderThrow

----------------------------------------------------------------------------
-- Initialization
----------------------------------------------------------------------------

prepareBlockDB
    :: forall ssc m.
       (Ssc ssc, MonadDB ssc m)
    => GenesisBlock ssc -> m ()
prepareBlockDB = putBlock (Undo [] [] def) . Left

----------------------------------------------------------------------------
-- Keys
----------------------------------------------------------------------------

blockKey :: HeaderHash -> ByteString
blockKey h = "b" <> convert h

undoKey :: HeaderHash -> ByteString
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
    => HeaderHash -> m (Block ssc, Undo)
getBlundThrow hash =
    maybeThrow (DBMalformed $ sformat errFmt hash) =<<
    (liftA2 (,) <$> getBlock hash <*> getUndo hash)
  where
    errFmt = ("getBlockThrow: no blund with HeaderHash: " %shortHashF)

getBlockThrow
    :: (Ssc ssc, MonadDB ssc m)
    => HeaderHash -> m (Block ssc)
getBlockThrow hash = maybeThrow (DBMalformed $ sformat errFmt hash) =<< getBlock hash
  where
    errFmt =
        ("getBlockThrow: no block with HeaderHash: "%shortHashF)

getHeaderThrow
    :: (Ssc ssc, MonadDB ssc m)
    => HeaderHash -> m (BlockHeader ssc)
getHeaderThrow hash = maybeThrow (DBMalformed $ sformat errFmt hash) =<< getBlockHeader hash
  where
    errFmt =
        ("getBlockThrow: no block header with hash: "%shortHashF)
