{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

-- | Interface to Blocks DB.

module Pos.DB.Block
       ( blkGetHeader
       , blkGetBlock
       , blkGetUndo
       , blkGetBlund

       , deleteBlock

       , prepareBlockDB

       -- * Load data
       , loadBlundsWhile
       , loadBlundsByDepth
       , loadBlocksWhile
       , loadHeadersWhile
       , loadHeadersByDepth
       , loadHeadersByDepthWhile

       -- * Pure implementation
       , dbGetHeaderPureDefault
       , dbGetBlockPureDefault
       , dbGetUndoPureDefault
       , dbPutBlundPureDefault

       -- * MonadBlockDB
       , MonadBlockDB
       , MonadBlockDBWrite
       , dbGetBlockDefault
       , dbGetUndoDefault
       , dbGetHeaderDefault
       , dbGetBlockSscDefault
       , dbGetUndoSscDefault
       , dbGetHeaderSscDefault
       , dbPutBlundDefault
       ) where

import           Universum

import           Control.Lens          (at, _Wrapped)
import           Data.ByteArray        (convert)
import qualified Data.ByteString       as BS (readFile, writeFile)
import           Data.Default          (Default (def))
import           Ether.Internal        (HasLens (..))
import           Formatting            (build, formatToString, sformat, (%))
import           System.Directory      (createDirectoryIfMissing, removeFile)
import           System.FilePath       ((</>))
import           System.IO.Error       (isDoesNotExistError)

import           Pos.Binary.Block      ()
import           Pos.Binary.Class      (Bi, decodeFull, decodeOrFail, encode)
import           Pos.Block.Core        (Block, BlockHeader, GenesisBlock)
import qualified Pos.Block.Core        as BC
import           Pos.Block.Types       (Blund, Undo (..))
import           Pos.Constants         (genesisHash)
import           Pos.Core              (BlockCount, HasDifficulty (difficultyL),
                                        HasPrevBlock (prevBlockL), HeaderHash, IsHeader,
                                        headerHash)
import           Pos.Crypto            (hashHexF, shortHashF)
import           Pos.DB.Class          (DBTag (..), MonadBlockDBGeneric (..),
                                        MonadBlockDBGenericWrite (..), MonadDBRead,
                                        dbGetBlund)
import           Pos.DB.Error          (DBError (DBMalformed))
import           Pos.DB.Functions      (dbGetBi)
import           Pos.DB.Pure           (DBPureVar, MonadPureDB, atomicModifyIORefPure,
                                        pureBlockIndexDB, pureBlocksStorage)
import           Pos.DB.Rocks          (MonadRealDB, blockDataDir, getBlockIndexDB,
                                        getNodeDBs, rocksDelete, rocksDelete, rocksPutBi)
import           Pos.Ssc.Class.Helpers (SscHelpersClass)
import           Pos.Ssc.Class.Types   (SscBlock)
import           Pos.Ssc.Util          (toSscBlock)
import           Pos.Util              (Some (..), maybeThrow)
import           Pos.Util.Chrono       (NewestFirst (..))

----------------------------------------------------------------------------
-- Implementations for 'MonadRealDB'
----------------------------------------------------------------------------

-- Get block with given hash from Block DB.  This function has too
-- strict constraint, consider using 'blkGetBlock'.

getBlock
    :: forall ssc ctx m. (SscHelpersClass ssc, MonadRealDB ctx m)
    => HeaderHash -> m (Maybe (Block ssc))
getBlock = blockDataPath >=> getData

-- | Returns header of block that was requested from Block DB.
blkGetHeader
    :: (SscHelpersClass ssc, MonadDBRead m)
    => HeaderHash -> m (Maybe (BlockHeader ssc))
blkGetHeader = dbGetBi BlockIndexDB . blockIndexKey

-- Get undo data for block with given hash from Block DB. This
-- function has too strict constraint, consider using 'blkGetUndo'.
getUndo :: (MonadRealDB ctx m) => HeaderHash -> m (Maybe Undo)
getUndo = undoDataPath >=> getData

-- Put given block, its metadata and Undo data into Block DB. This
-- function uses 'MonadRealDB' constraint which is too
-- severe. Consider using 'dbPutBlund' instead.
putBlundReal
    :: (SscHelpersClass ssc, MonadRealDB ctx m)
    => Blund ssc -> m ()
putBlundReal (blk, undo) = do
    let h = headerHash blk
    liftIO . createDirectoryIfMissing False =<< dirDataPath h
    flip putData blk =<< blockDataPath h
    flip putData undo =<< undoDataPath h
    putBi (blockIndexKey h) (BC.getBlockHeader blk)

deleteBlock :: (MonadRealDB ctx m) => HeaderHash -> m ()
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
    -> BlockCount
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
    :: (MonadBlockDB ssc m)
    => (Block ssc -> Bool) -> HeaderHash -> m (NewestFirst [] (Blund ssc))
loadBlundsWhile predicate = loadDataWhile getBlundThrow (predicate . fst)

-- | Load blunds which have depth less than given (depth = number of
-- blocks that will be returned).
loadBlundsByDepth
    :: (MonadBlockDB ssc m)
    => BlockCount -> HeaderHash -> m (NewestFirst [] (Blund ssc))
loadBlundsByDepth = loadDataByDepth getBlundThrow (const True)

-- | Load blocks starting from block with header hash equal to given hash
-- and while @predicate@ is true.
loadBlocksWhile
    :: (MonadBlockDB ssc m)
    => (Block ssc -> Bool) -> HeaderHash -> m (NewestFirst [] (Block ssc))
loadBlocksWhile = loadDataWhile getBlockThrow

-- | Load headers starting from block with header hash equal to given hash
-- and while @predicate@ is true.
loadHeadersWhile
    :: (SscHelpersClass ssc, MonadDBRead m)
    => (BlockHeader ssc -> Bool)
    -> HeaderHash
    -> m (NewestFirst [] (BlockHeader ssc))
loadHeadersWhile = loadDataWhile getHeaderThrow

-- | Load headers which have depth less than given.
loadHeadersByDepth
    :: (SscHelpersClass ssc, MonadDBRead m)
    => BlockCount -> HeaderHash -> m (NewestFirst [] (BlockHeader ssc))
loadHeadersByDepth = loadDataByDepth getHeaderThrow (const True)

-- | Load headers which have depth less than given and match some criterion.
loadHeadersByDepthWhile
    :: (SscHelpersClass ssc, MonadDBRead m)
    => (BlockHeader ssc -> Bool)
    -> BlockCount
    -> HeaderHash
    -> m (NewestFirst [] (BlockHeader ssc))
loadHeadersByDepthWhile = loadDataByDepth getHeaderThrow

----------------------------------------------------------------------------
-- Pure implementation
----------------------------------------------------------------------------

decodeOrFailPureDB :: SscHelpersClass ssc => ByteString -> (Block ssc, Undo)
decodeOrFailPureDB = decodeOrFail

dbGetBlundPure ::
       forall ssc ctx m. (MonadPureDB ctx m, SscHelpersClass ssc)
    => HeaderHash
    -> m (Maybe (Block ssc, Undo))
dbGetBlundPure h = do
    (blund :: Maybe ByteString) <-
        view (pureBlocksStorage . at h) <$> (view (lensOf @DBPureVar) >>= readIORef)
    pure $ decodeOrFailPureDB <$> blund

dbGetBlockPureDefault ::
       forall ssc ctx m. (MonadPureDB ctx m, SscHelpersClass ssc)
    => HeaderHash
    -> m (Maybe (Block ssc))
dbGetBlockPureDefault h = fmap fst <$> dbGetBlundPure h

dbGetUndoPureDefault ::
       forall ssc ctx m. (MonadPureDB ctx m, SscHelpersClass ssc)
    => HeaderHash
    -> m (Maybe Undo)
dbGetUndoPureDefault h = fmap snd <$> dbGetBlundPure @ssc @ctx @m h

dbGetHeaderPureDefault ::
       forall ssc m. (MonadDBRead m, SscHelpersClass ssc)
    => HeaderHash
    -> m (Maybe (BlockHeader ssc))
dbGetHeaderPureDefault = blkGetHeader

dbPutBlundPureDefault ::
       forall ssc ctx m. (MonadPureDB ctx m, SscHelpersClass ssc)
    => Blund ssc
    -> m ()
dbPutBlundPureDefault (blk,undo) = do
    let h = headerHash blk
    (var :: DBPureVar) <- view (lensOf @DBPureVar)
    flip atomicModifyIORefPure var $
        (pureBlocksStorage . at h .~ Just (encode (blk,undo))) .
        (pureBlockIndexDB . at (blockIndexKey h) .~ Just (encode $ BC.getBlockHeader blk))

----------------------------------------------------------------------------
-- Initialization
----------------------------------------------------------------------------

prepareBlockDB
    :: forall ssc m.
       MonadBlockDBWrite ssc m
    => GenesisBlock ssc -> m ()
prepareBlockDB blk = dbPutBlund @(BlockHeader ssc) @(Block ssc) @Undo (Left blk, def)

----------------------------------------------------------------------------
-- Keys
----------------------------------------------------------------------------

blockIndexKey :: HeaderHash -> ByteString
blockIndexKey h = "b" <> convert h

----------------------------------------------------------------------------
-- MonadBlockDB related
----------------------------------------------------------------------------

-- | Specialization of 'MonadBlockDBGeneric' for block processing.
type MonadBlockDB ssc m
     = ( MonadBlockDBGeneric (BlockHeader ssc) (Block ssc) Undo m
       , MonadBlockDBGeneric (Some IsHeader) (SscBlock ssc) () m
       , SscHelpersClass ssc)

-- | 'MonadBlocksDB' with write options
type MonadBlockDBWrite ssc m
    = ( MonadBlockDB ssc m
      , MonadBlockDBGenericWrite (BlockHeader ssc) (Block ssc) Undo m
      )

-- instance MonadBlockDBGeneric (Block ssc)

type BlockDBGenericDefaultEnv ssc ctx m =
    (MonadDBRead m, MonadRealDB ctx m, SscHelpersClass ssc)

dbGetBlockDefault ::
       forall ssc ctx m. BlockDBGenericDefaultEnv ssc ctx m
    => HeaderHash
    -> m (Maybe (Block ssc))
dbGetBlockDefault = getBlock

dbGetUndoDefault ::
       forall ssc ctx m. BlockDBGenericDefaultEnv ssc ctx m
    => HeaderHash
    -> m (Maybe Undo)
dbGetUndoDefault = getUndo

dbGetHeaderDefault ::
       forall ssc ctx m. BlockDBGenericDefaultEnv ssc ctx m
    => HeaderHash
    -> m (Maybe (BlockHeader ssc))
dbGetHeaderDefault = blkGetHeader

dbGetBlockSscDefault ::
       forall ssc ctx m. BlockDBGenericDefaultEnv ssc ctx m
    => HeaderHash
    -> m (Maybe (SscBlock ssc))
dbGetBlockSscDefault = fmap (toSscBlock <$>) . getBlock

dbGetUndoSscDefault ::
       forall ssc ctx m. BlockDBGenericDefaultEnv ssc ctx m
    => HeaderHash
    -> m (Maybe ())
dbGetUndoSscDefault = fmap (const () <$>) . getUndo

dbGetHeaderSscDefault ::
       forall ssc ctx m. BlockDBGenericDefaultEnv ssc ctx m
    => HeaderHash
    -> m (Maybe (Some IsHeader))
dbGetHeaderSscDefault = fmap (Some <$>) . blkGetHeader @ssc

-- instance MonadBlockDBWrite

dbPutBlundDefault :: (MonadDBRead m, MonadRealDB ctx m, SscHelpersClass ssc) => Blund ssc -> m ()
dbPutBlundDefault = putBlundReal

-- helpers

blkGetBlock ::
       forall ssc m. MonadBlockDB ssc m
    => HeaderHash
    -> m $ Maybe (Block ssc)
blkGetBlock = dbGetBlock @(BlockHeader ssc) @(Block ssc) @Undo

blkGetUndo ::
       forall ssc m. MonadBlockDB ssc m
    => HeaderHash
    -> m $ Maybe Undo
blkGetUndo = dbGetUndo @(BlockHeader ssc) @(Block ssc) @Undo

blkGetBlund ::
       forall ssc m. MonadBlockDB ssc m
    => HeaderHash
    -> m $ Maybe (Blund ssc)
blkGetBlund = dbGetBlund @(BlockHeader ssc) @(Block ssc) @Undo

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

putBi
    :: (MonadRealDB ctx m, Bi v)
    => ByteString -> v -> m ()
putBi k v = rocksPutBi k v =<< getBlockIndexDB

delete :: (MonadRealDB ctx m) => ByteString -> m ()
delete k = rocksDelete k =<< getBlockIndexDB

getData ::  (MonadIO m, MonadCatch m, Bi v) => FilePath -> m (Maybe v)
getData fp = flip catch handle $ liftIO $
    either (\er -> throwM $ DBMalformed $
             sformat ("Couldn't deserialize "%build%", reason: "%build) fp er) pure .
    decodeFull <$>
    BS.readFile fp
  where
    handle e
        | isDoesNotExistError e = pure Nothing
        | otherwise = throwM e

putData ::  (MonadIO m, Bi v) => FilePath -> v -> m ()
putData fp = liftIO . BS.writeFile fp . encode

deleteData :: (MonadIO m, MonadCatch m) => FilePath -> m ()
deleteData fp = (liftIO $ removeFile fp) `catch` handle
  where
    handle e
        | isDoesNotExistError e = pure ()
        | otherwise = throwM e

dirDataPath :: MonadRealDB ctx m => HeaderHash -> m FilePath
dirDataPath (formatToString hashHexF -> fn) = gitDirDataPath fn

blockDataPath :: MonadRealDB ctx m => HeaderHash -> m FilePath
blockDataPath (formatToString (hashHexF%".block") -> fn) =
    gitDirDataPath fn <&> (</> drop 2 fn)

undoDataPath :: MonadRealDB ctx m => HeaderHash -> m FilePath
undoDataPath (formatToString (hashHexF%".undo") -> fn) =
    gitDirDataPath fn <&> (</> drop 2 fn)

gitDirDataPath :: MonadRealDB ctx m => [Char] -> m FilePath
gitDirDataPath fn = getNodeDBs <&> \dbs -> dbs ^. blockDataDir </> take 2 fn

----------------------------------------------------------------------------
-- Private functions
----------------------------------------------------------------------------

getBlundThrow
    :: forall ssc m. MonadBlockDB ssc m
    => HeaderHash -> m (Blund ssc)
getBlundThrow hash =
    maybeThrow (DBMalformed $ sformat errFmt hash) =<< (blkGetBlund @ssc) hash
  where
    errFmt = "getBlockThrow: no blund with HeaderHash: "%shortHashF

getBlockThrow
    :: forall ssc m. MonadBlockDB ssc m
    => HeaderHash -> m (Block ssc)
getBlockThrow hash =
    maybeThrow (DBMalformed $ sformat errFmt hash) =<< blkGetBlock hash
  where
    errFmt = "getBlockThrow: no block with HeaderHash: "%shortHashF

getHeaderThrow
    :: (SscHelpersClass ssc, MonadDBRead m)
    => HeaderHash -> m (BlockHeader ssc)
getHeaderThrow hash =
    maybeThrow (DBMalformed $ sformat errFmt hash) =<< blkGetHeader hash
  where
    errFmt = "getBlockThrow: no block header with hash: "%shortHashF
