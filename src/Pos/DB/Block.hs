{-# LANGUAGE ScopedTypeVariables #-}

-- | Interface to Blocks DB.

module Pos.DB.Block
       ( getBlock
       , getBlockHeader
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

import           Control.Lens              (_Wrapped)
import           Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import           Data.ByteArray            (convert)
import qualified Data.ByteString           as BS (readFile, writeFile)
import qualified Data.ByteString.Lazy      as BSL
import           Data.Default              (Default (def))
import           Formatting                (build, formatToString, sformat, (%))
import           System.Directory          (removeFile, createDirectoryIfMissing)
import           System.FilePath           ((</>))
import           System.IO.Error           (isDoesNotExistError)
import           Universum

import           Pos.Binary.Block          ()
import           Pos.Binary.Class          (Bi, decodeFull, encodeStrict)
import           Pos.Block.Core            (Block, BlockHeader, GenesisBlock)
import qualified Pos.Block.Core            as BC
import           Pos.Block.Pure            (genesisHash)
import           Pos.Block.Types           (Blund, Undo (..))
import           Pos.Core                  (HasDifficulty (difficultyL),
                                            HasPrevBlock (prevBlockL), HeaderHash,
                                            headerHash)
import           Pos.Crypto                (hashHexF, shortHashF)
import           Pos.DB.Class              (MonadDB, getBlockIndexDB, getNodeDBs)
import           Pos.DB.Error              (DBError (DBMalformed))
import           Pos.DB.Functions          (rocksDelete, rocksGetBi, rocksPutBi)
import           Pos.DB.Types              (blockDataDir)
import           Pos.Ssc.Class.Helpers     (SscHelpersClass)
import           Pos.Util                  (maybeThrow)
import           Pos.Util.Chrono           (NewestFirst (..))

-- | Get block with given hash from Block DB.
getBlock
    :: forall ssc m. (SscHelpersClass ssc, MonadDB m)
    => HeaderHash -> m (Maybe (Block ssc))
getBlock = blockDataPath >=> getData

-- | Returns header of block that was requested from Block DB.
getBlockHeader
    :: (SscHelpersClass ssc, MonadDB m)
    => HeaderHash -> m (Maybe (BlockHeader ssc))
getBlockHeader = getBi . blockIndexKey

-- | Get undo data for block with given hash from Block DB.
getUndo :: (MonadDB m) => HeaderHash -> m (Maybe Undo)
getUndo = undoDataPath >=> getData

-- | Retrieves block and undo together.
getBlockWithUndo
    :: (SscHelpersClass ssc, MonadDB m)
    => HeaderHash -> m (Maybe (Block ssc, Undo))
getBlockWithUndo x =
    runMaybeT $ (,) <$> MaybeT (getBlock x) <*> MaybeT (getUndo x)

-- | Put given block, its metadata and Undo data into Block DB.
putBlock
    :: (SscHelpersClass ssc, MonadDB m)
    => Undo -> Block ssc -> m ()
putBlock undo blk = do
    let h = headerHash blk
    liftIO . createDirectoryIfMissing False =<< dirDataPath h
    flip putData blk =<< blockDataPath h
    flip putData undo =<< undoDataPath h
    putBi (blockIndexKey h) (BC.getBlockHeader blk)

deleteBlock :: (MonadDB m) => HeaderHash -> m ()
deleteBlock hh = do
    delete (blockIndexKey hh)
    deleteData =<< blockDataPath hh
    deleteData =<< undoDataPath hh

----------------------------------------------------------------------------
-- Load
----------------------------------------------------------------------------

loadDataWhile
    :: forall m a .
       (Monad m, HasPrevBlock a)
    => (HeaderHash -> m a)
    -> (a -> Bool)
    -> HeaderHash
    -> m (NewestFirst [] a)
loadDataWhile getter predicate start = NewestFirst <$> doIt start
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
    => (HeaderHash -> m a)
    -> (a -> Bool)
    -> Word
    -> HeaderHash
    -> m (NewestFirst [] a)
loadDataByDepth _ _ 0 _ = pure (NewestFirst [])
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
    over _Wrapped (top :) <$>
        loadDataWhile
        getter
        (\a -> a ^. difficultyL >= targetDifficulty && extraPredicate a)
        prev

-- | Load blunds starting from block with header hash equal to given hash
-- and while @predicate@ is true.
loadBlundsWhile
    :: (SscHelpersClass ssc, MonadDB m)
    => (Block ssc -> Bool) -> HeaderHash -> m (NewestFirst [] (Blund ssc))
loadBlundsWhile predicate = loadDataWhile getBlundThrow (predicate . fst)

-- | Load blunds which have depth less than given.
loadBlundsByDepth
    :: (SscHelpersClass ssc, MonadDB m)
    => Word -> HeaderHash -> m (NewestFirst [] (Blund ssc))
loadBlundsByDepth = loadDataByDepth getBlundThrow (const True)

-- | Load blocks starting from block with header hash equal to given hash
-- and while @predicate@ is true.
loadBlocksWhile
    :: (SscHelpersClass ssc, MonadDB m)
    => (Block ssc -> Bool) -> HeaderHash -> m (NewestFirst [] (Block ssc))
loadBlocksWhile = loadDataWhile getBlockThrow

-- | Load headers starting from block with header hash equal to given hash
-- and while @predicate@ is true.
loadHeadersWhile
    :: (SscHelpersClass ssc, MonadDB m)
    => (BlockHeader ssc -> Bool)
    -> HeaderHash
    -> m (NewestFirst [] (BlockHeader ssc))
loadHeadersWhile = loadDataWhile getHeaderThrow

-- | Load headers which have depth less than given.
loadHeadersByDepth
    :: (SscHelpersClass ssc, MonadDB m)
    => Word -> HeaderHash -> m (NewestFirst [] (BlockHeader ssc))
loadHeadersByDepth = loadDataByDepth getHeaderThrow (const True)

-- | Load headers which have depth less than given and match some criterion.
loadHeadersByDepthWhile
    :: (SscHelpersClass ssc, MonadDB m)
    => (BlockHeader ssc -> Bool)
    -> Word
    -> HeaderHash
    -> m (NewestFirst [] (BlockHeader ssc))
loadHeadersByDepthWhile = loadDataByDepth getHeaderThrow

----------------------------------------------------------------------------
-- Initialization
----------------------------------------------------------------------------

prepareBlockDB
    :: forall ssc m.
       (SscHelpersClass ssc, MonadDB m)
    => GenesisBlock ssc -> m ()
prepareBlockDB = putBlock def . Left

----------------------------------------------------------------------------
-- Keys
----------------------------------------------------------------------------

blockIndexKey :: HeaderHash -> ByteString
blockIndexKey h = "b" <> convert h

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

getBi
    :: (MonadDB m, Bi v)
    => ByteString -> m (Maybe v)
getBi k = rocksGetBi k =<< getBlockIndexDB

putBi
    :: (MonadDB m, Bi v)
    => ByteString -> v -> m ()
putBi k v = rocksPutBi k v =<< getBlockIndexDB

delete :: (MonadDB m) => ByteString -> m ()
delete k = rocksDelete k =<< getBlockIndexDB

getData ::  (MonadIO m, MonadCatch m, Bi v) => FilePath -> m (Maybe v)
getData fp = flip catch handle $ liftIO $
    either (\er -> throwM $ DBMalformed $
             sformat ("Couldn't deserialize "%build%", reason: "%build) fp er) pure .
    decodeFull .
    BSL.fromStrict <$>
    BS.readFile fp
  where
    handle e
        | isDoesNotExistError e = pure Nothing
        | otherwise = throwM e

putData ::  (MonadIO m, Bi v) => FilePath -> v -> m ()
putData fp = liftIO . BS.writeFile fp . encodeStrict

deleteData :: (MonadIO m, MonadCatch m) => FilePath -> m ()
deleteData fp = (liftIO $ removeFile fp) `catch` handle
  where
    handle e
        | isDoesNotExistError e = pure ()
        | otherwise = throwM e

dirDataPath :: MonadDB m => HeaderHash -> m FilePath
dirDataPath (formatToString hashHexF -> fn) = gitDirDataPath fn

blockDataPath :: MonadDB m => HeaderHash -> m FilePath
blockDataPath (formatToString (hashHexF%".block") -> fn) =
    gitDirDataPath fn <&> (</> drop 2 fn)

undoDataPath :: MonadDB m => HeaderHash -> m FilePath
undoDataPath (formatToString (hashHexF%".undo") -> fn) =
    gitDirDataPath fn <&> (</> drop 2 fn)

gitDirDataPath :: MonadDB m => [Char] -> m FilePath
gitDirDataPath fn = getNodeDBs <&> \dbs -> dbs ^. blockDataDir </> take 2 fn

----------------------------------------------------------------------------
-- Private functions
----------------------------------------------------------------------------

getBlundThrow
    :: (SscHelpersClass ssc, MonadDB m)
    => HeaderHash -> m (Block ssc, Undo)
getBlundThrow hash =
    maybeThrow (DBMalformed $ sformat errFmt hash) =<<
    (liftA2 (,) <$> getBlock hash <*> getUndo hash)
  where
    errFmt = "getBlockThrow: no blund with HeaderHash: " %shortHashF

getBlockThrow
    :: (SscHelpersClass ssc, MonadDB m)
    => HeaderHash -> m (Block ssc)
getBlockThrow hash = maybeThrow (DBMalformed $ sformat errFmt hash) =<< getBlock hash
  where
    errFmt = "getBlockThrow: no block with HeaderHash: "%shortHashF

getHeaderThrow
    :: (SscHelpersClass ssc, MonadDB m)
    => HeaderHash -> m (BlockHeader ssc)
getHeaderThrow hash = maybeThrow (DBMalformed $ sformat errFmt hash) =<< getBlockHeader hash
  where
    errFmt = "getBlockThrow: no block header with hash: "%shortHashF
