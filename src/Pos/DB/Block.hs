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
       , loadBlundsWhile
       , loadBlocksWhile
       , loadHeadersWhile

       , prepareBlockDB
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
import           Pos.Types           (Block, BlockHeader, GenesisBlock, HasPrevBlock,
                                      HeaderHash, Undo (..), blockHeader, genesisHash,
                                      headerHash, prevBlockL)
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

loadDataWhile
    :: forall m a b.
       (Monad m, HasPrevBlock a b)
    => (Hash b -> m a) -> (a -> Int -> Bool) -> Hash b -> m [a]
loadDataWhile getter predicate start = doIt 0 start
  where
    doIt :: Int -> Hash b -> m [a]
    doIt depth h
        | h == genesisHash = pure []
        | otherwise = do
            d <- getter h
            let prev = d ^. prevBlockL
            if predicate d depth
                then (d :) <$> doIt (succ depth) prev
                else pure []
    -- depthDelta (Left _)  = 0
    -- depthDelta (Right _) = 1

-- | Load blunds starting from block with header hash equal to given hash
-- and while @predicate@ is true.  The head of returned list is the
-- youngest blund.
loadBlundsWhile
    :: (Ssc ssc, MonadDB ssc m)
    => (Block ssc -> Int -> Bool) -> HeaderHash ssc -> m [(Block ssc, Undo)]
loadBlundsWhile predicate = loadDataWhile getBlundThrow (predicate . fst)

-- | Load blocks starting from block with header hash equal to given hash
-- and while @predicate@ is true.  The head of returned list is the
-- youngest block.
loadBlocksWhile
    :: (Ssc ssc, MonadDB ssc m)
    => (Block ssc -> Int -> Bool) -> HeaderHash ssc -> m [Block ssc]
loadBlocksWhile = loadDataWhile getBlockThrow

-- | Load headers starting from block with header hash equal to given hash
-- and while @predicate@ is true.  The head of returned list is the
-- youngest header.
loadHeadersWhile
    :: (Ssc ssc, MonadDB ssc m)
    => (BlockHeader ssc -> Int -> Bool) -> HeaderHash ssc -> m [BlockHeader ssc]
loadHeadersWhile predicate h =
    map (view blockHeader) <$>
    loadDataWhile getBlockThrow (predicate . view blockHeader) h

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
