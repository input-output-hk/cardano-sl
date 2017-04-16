{-# LANGUAGE ScopedTypeVariables #-}

-- | Interface to Blocks DB.
-- There is block index which stores header of block by header hash.
-- Blocks and its undos (blund) are stored in plain files,
-- several blund per file due to avoid fragmentation.
-- We don't store blocks in rocksdb for easy portability between databases.

module Pos.DB.Block.Block
       ( getBlock
       , getBlockHeader
       , getUndo
       , getBlund
       , putBlund

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
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Lazy      as BSL
import           Data.Default              (Default (def))
import           Formatting                (build, formatToString, int, sformat, (%))
import           System.FilePath           ((</>))
import           System.IO                 (IOMode (ReadMode), SeekMode (AbsoluteSeek),
                                            hFileSize, hSeek, withFile)
import           Universum

import           Pos.Binary.Block          ()
import           Pos.Binary.Class          (Bi, FixedSizeInt (..), WithLength (..),
                                            decodeFull, encodeStrict)
import           Pos.Binary.DB             ()
import           Pos.Block.Types           (Blund, Undo (..))
import           Pos.Constants             (maxBlundFileSize)
import           Pos.Crypto                (shortHashF)
import           Pos.DB.Block.Aux          (BlundLocation (..))
import           Pos.DB.Class              (MonadDB, getBlockIndexDB, getNodeDBs)
import           Pos.DB.Error              (DBError (DBMalformed))
import           Pos.DB.Functions          (rocksGetBi, rocksPutBi)
import           Pos.DB.Types              (blockDataDir)
import           Pos.Ssc.Class.Helpers     (SscHelpersClass)
import           Pos.Types                 (Block, BlockHeader, GenesisBlock,
                                            HasDifficulty (difficultyL), HasPrevBlock,
                                            HeaderHash, genesisHash, headerHash,
                                            prevBlockL)
import qualified Pos.Types                 as T
import           Pos.Util                  (maybeThrow)
import           Pos.Util.Chrono           (NewestFirst (..))

-- | Get block with given hash from Block DB.
getBlock
    :: (SscHelpersClass ssc, MonadDB m)
    => HeaderHash -> m (Maybe (Block ssc))
getBlock hh = getBi (blockAuxKey hh) >>= \case
    Nothing -> pure Nothing
    Just BlundLocation{..} -> blundDataPath blundFileNum >>= fmap Just . getData blockOffset blockLen

-- | Returns header of block that was requested from Block DB.
getBlockHeader
    :: (SscHelpersClass ssc, MonadDB m)
    => HeaderHash -> m (Maybe (BlockHeader ssc))
getBlockHeader = getBi . blockIndexKey

-- | Get undo data for block with given hash from Block DB.
getUndo :: (MonadDB m) => HeaderHash -> m (Maybe Undo)
getUndo hh = getBi (blockAuxKey hh) >>= \case
    Nothing -> pure Nothing
    Just BlundLocation{..} -> blundDataPath blundFileNum >>= fmap Just . getData undoOffset undoLen

-- | Retrieves block and undo together.
getBlund
    :: (SscHelpersClass ssc, MonadDB m)
    => HeaderHash -> m (Maybe (Block ssc, Undo))
getBlund x =
    runMaybeT $ (,) <$> MaybeT (getBlock x) <*> MaybeT (getUndo x)

-- | Put given block, its metadata and Undo data into Block DB.
putBlund
    :: (SscHelpersClass ssc, MonadDB m)
    => Block ssc -> Undo -> m ()
putBlund blk undo = getBi lastBlundFileKey >>= \case
    Nothing -> throwM $ DBMalformed "No last block file in RocksDB"
    Just (FixedSizeInt (lastFile::Int)) -> do
        let h = headerHash blk
        let blundFileNum = fromIntegral lastFile
        bDataPath <- blundDataPath blundFileNum
        (blockOffset, blockLen) <- putData blk bDataPath
        (undoOffset, undoLen) <- putData undo bDataPath
        let blockAux = BlundLocation{..}
        putBi (blockAuxKey h) blockAux
        putBi (blockIndexKey h) (T.getBlockHeader blk)
        when (undoOffset + undoLen > maxBlundFileSize) $
            putBi lastBlundFileKey (FixedSizeInt $ lastFile + 1)

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
loadDataWhile getter predicate start = NewestFirst <$> go start
  where
    go :: HeaderHash -> m [a]
    go h
        | h == genesisHash = pure []
        | otherwise = do
            d <- getter h
            let prev = d ^. prevBlockL
            if predicate d
                then (d :) <$> go prev
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
prepareBlockDB g = do
    putBi lastBlundFileKey (FixedSizeInt (0::Int))
    putBlund (Left g) def

----------------------------------------------------------------------------
-- Keys
----------------------------------------------------------------------------

-- | DB key for getting block header
blockIndexKey :: HeaderHash -> ByteString
blockIndexKey h = "b" <> convert h

-- | DB key for location of given block
blockAuxKey :: HeaderHash -> ByteString
blockAuxKey h = "a" <> convert h

-- | Last used number for blund file
lastBlundFileKey :: ByteString
lastBlundFileKey = "last_blund_file"

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

-- delete :: (MonadDB m) => ByteString -> m ()
-- delete k = rocksDelete k =<< getBlockIndexDB

getData :: (MonadIO m, MonadCatch m, Bi v)
        => Word32 -- Offset
        -> Word32 -- Length
        -> FilePath
        -> m v
getData offset len fp = liftIO $ withFile fp ReadMode $ \h -> do
    hSeek h AbsoluteSeek (fromIntegral offset)
    bs <- BS.hGet h (fromIntegral len)
    either throwDB (pure . unWithLength) . decodeFull . BSL.fromStrict $ bs
  where
    throwDB er = throwM $ DBMalformed $
      sformat ("Couldn't deserialize "%build%
               ", offset: "%build%
               ", len: "%build%
               ", reason: "%build) fp offset len er

-- Serialize data with length to be able read data sequentially without rocksdb index
putData :: (MonadIO m, Bi v)
        => v
        -> FilePath
        -> m (Word32, Word32) -- Return (offset, len)
putData val fp = liftIO $ withFile fp AppendMode $ \h -> do
    offset <- hFileSize h
    let bs = encodeStrict $ WithLength val
    let bsLen = BS.length bs
    BS.hPut h bs
    pure (fromIntegral offset, fromIntegral bsLen)

blundDataPath :: MonadDB m => Word32 -> m FilePath
blundDataPath (formatToString (int%".blunds") -> fn) =
    getNodeDBs <&> \dbs -> dbs ^. blockDataDir </> fn

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
    errFmt = ("getBlockThrow: no blund with HeaderHash: " %shortHashF)

getBlockThrow
    :: (SscHelpersClass ssc, MonadDB m)
    => HeaderHash -> m (Block ssc)
getBlockThrow hash = maybeThrow (DBMalformed $ sformat errFmt hash) =<< getBlock hash
  where
    errFmt =
        ("getBlockThrow: no block with HeaderHash: "%shortHashF)

getHeaderThrow
    :: (SscHelpersClass ssc, MonadDB m)
    => HeaderHash -> m (BlockHeader ssc)
getHeaderThrow hash = maybeThrow (DBMalformed $ sformat errFmt hash) =<< getBlockHeader hash
  where
    errFmt =
        ("getBlockThrow: no block header with hash: "%shortHashF)
