{-# LANGUAGE AllowAmbiguousTypes #-}
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

       -- * MonadBlockDB
       , MonadBlockDB
       , MonadSscBlockDB
       , MonadBlockDBWrite

       -- * Pure implementation
       , dbGetHeaderPureDefault
       , dbGetBlockPureDefault
       , dbGetUndoPureDefault
       , dbGetBlockSscPureDefault
       , dbGetUndoSscPureDefault
       , dbGetHeaderSscPureDefault
       , dbPutBlundPureDefault

       -- * Rocks implementation
       , dbGetBlockDefault
       , dbGetUndoDefault
       , dbGetHeaderDefault
       , dbGetBlockSscDefault
       , dbGetUndoSscDefault
       , dbGetHeaderSscDefault
       , dbPutBlundDefault

       -- * DBSum implementation
       , dbGetBlockSumDefault
       , dbGetUndoSumDefault
       , dbGetHeaderSumDefault
       , dbGetBlockSscSumDefault
       , dbGetUndoSscSumDefault
       , dbGetHeaderSscSumDefault
       , dbPutBlundSumDefault
       ) where

import           Universum

import           Control.Exception.Safe (handle)
import           Control.Lens           (at, _Wrapped)
import           Data.ByteArray         (convert)
import qualified Data.ByteString        as BS (hPut, readFile)
import           Data.Default           (Default (def))
import           Ether.Internal         (HasLens (..))
import           Formatting             (build, formatToString, sformat, (%))
import           System.Directory       (createDirectoryIfMissing, removeFile)
import           System.FilePath        ((</>))
import           System.IO              (IOMode (WriteMode), hClose, hFlush,
                                         openBinaryFile)
import           System.IO.Error        (IOError, isDoesNotExistError)

import           Pos.Binary.Block       ()
import           Pos.Binary.Class       (Bi, decodeFull, serialize')
import           Pos.Block.Core         (Block, BlockHeader, GenesisBlock)
import qualified Pos.Block.Core         as BC
import           Pos.Block.Types        (Blund, SlogUndo (..), Undo (..))
import           Pos.Core               (BlockCount, HasConfiguration,
                                         HasDifficulty (difficultyL),
                                         HasPrevBlock (prevBlockL), HeaderHash, IsHeader,
                                         headerHash)
import           Pos.Core.Configuration (genesisHash)
import           Pos.Crypto             (hashHexF, shortHashF)
import           Pos.DB.Class           (DBTag (..), MonadBlockDBGeneric (..),
                                         MonadBlockDBGenericWrite (..), MonadDBRead,
                                         dbGetBlund)
import           Pos.DB.Error           (DBError (..))
import           Pos.DB.Functions       (dbGetBi, dbSerializeValue)
import           Pos.DB.Pure            (DBPureVar, MonadPureDB, atomicModifyIORefPure,
                                         pureBlockIndexDB, pureBlocksStorage)
import           Pos.DB.Rocks           (MonadRealDB, blockDataDir, getBlockIndexDB,
                                         getNodeDBs, rocksDelete, rocksPutBi)
import           Pos.DB.Sum             (MonadDBSum, eitherDB)
import           Pos.Delegation.Types   (DlgUndo (..))
import           Pos.Ssc                (SscBlock)
import           Pos.Ssc.Util           (toSscBlock)
import           Pos.Util               (Some (..), maybeThrow)
import           Pos.Util.Chrono        (NewestFirst (..))

----------------------------------------------------------------------------
-- Implementations for 'MonadRealDB'
----------------------------------------------------------------------------

-- Get block with given hash from Block DB.  This function has too
-- strict constraint, consider using 'blkGetBlock'.

getBlock
    :: forall ctx m. (HasConfiguration, MonadRealDB ctx m)
    => HeaderHash -> m (Maybe Block)
getBlock = blockDataPath >=> getData

-- | Returns header of block that was requested from Block DB.
blkGetHeader
    :: (HasConfiguration, MonadDBRead m)
    => HeaderHash -> m (Maybe BlockHeader)
blkGetHeader = dbGetBi BlockIndexDB . blockIndexKey

-- Get undo data for block with given hash from Block DB. This
-- function has too strict constraint, consider using 'blkGetUndo'.
getUndo :: (HasConfiguration, MonadRealDB ctx m) => HeaderHash -> m (Maybe Undo)
getUndo = undoDataPath >=> getData

-- Put given block, its metadata and Undo data into Block DB. This
-- function uses 'MonadRealDB' constraint which is too
-- severe. Consider using 'dbPutBlund' instead.
putBlundReal
    :: (HasConfiguration, MonadRealDB ctx m)
    => Blund -> m ()
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
       (Monad m, HasPrevBlock a, HasConfiguration)
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
       (Monad m, HasPrevBlock a, HasDifficulty a, HasConfiguration)
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
    :: (MonadBlockDB m)
    => (Block -> Bool) -> HeaderHash -> m (NewestFirst [] Blund)
loadBlundsWhile predicate = loadDataWhile getBlundThrow (predicate . fst)

-- | Load blunds which have depth less than given (depth = number of
-- blocks that will be returned).
loadBlundsByDepth
    :: (MonadBlockDB m)
    => BlockCount -> HeaderHash -> m (NewestFirst [] Blund)
loadBlundsByDepth = loadDataByDepth getBlundThrow (const True)

-- | Load blocks starting from block with header hash equal to given hash
-- and while @predicate@ is true.
loadBlocksWhile
    :: (MonadBlockDB m)
    => (Block -> Bool) -> HeaderHash -> m (NewestFirst [] Block)
loadBlocksWhile = loadDataWhile getBlockThrow

-- | Load headers starting from block with header hash equal to given hash
-- and while @predicate@ is true.
loadHeadersWhile
    :: (HasConfiguration, MonadDBRead m)
    => (BlockHeader -> Bool)
    -> HeaderHash
    -> m (NewestFirst [] BlockHeader)
loadHeadersWhile = loadDataWhile getHeaderThrow

-- | Load headers which have depth less than given.
loadHeadersByDepth
    :: (MonadDBRead m, HasConfiguration)
    => BlockCount -> HeaderHash -> m (NewestFirst [] BlockHeader)
loadHeadersByDepth = loadDataByDepth getHeaderThrow (const True)

-- | Load headers which have depth less than given and match some criterion.
loadHeadersByDepthWhile
    :: (MonadDBRead m, HasConfiguration)
    => (BlockHeader -> Bool)
    -> BlockCount
    -> HeaderHash
    -> m (NewestFirst [] BlockHeader)
loadHeadersByDepthWhile = loadDataByDepth getHeaderThrow

----------------------------------------------------------------------------
-- Initialization
----------------------------------------------------------------------------

prepareBlockDB
    :: (MonadBlockDBWrite m)
    => GenesisBlock -> m ()
prepareBlockDB blk =
    dbPutBlund @BlockHeader @Block @Undo (Left blk, genesisUndo)
  where
    genesisUndo =
        Undo
        { undoTx = mempty
        , undoDlg = DlgUndo mempty mempty
        , undoUS = def
        , undoSlog = SlogUndo Nothing
        }

----------------------------------------------------------------------------
-- Keys
----------------------------------------------------------------------------

blockIndexKey :: HeaderHash -> ByteString
blockIndexKey h = "b" <> convert h

----------------------------------------------------------------------------
-- MonadBlockDB related
----------------------------------------------------------------------------

-- | Specialization of 'MonadBlockDBGeneric' for block processing.
type MonadBlockDB m
     = ( MonadBlockDBGeneric BlockHeader Block Undo m )

type MonadSscBlockDB m
     = ( MonadBlockDBGeneric (Some IsHeader) SscBlock () m )

-- | 'MonadBlockDB' with write options
type MonadBlockDBWrite m
    = ( MonadBlockDB m
      , MonadBlockDBGenericWrite BlockHeader Block Undo m
      )

----------------------------------------------------------------------------
-- Pure implementation
----------------------------------------------------------------------------

decodeOrFailPureDB
    :: HasConfiguration
    => ByteString
    -> Either Text (Block, Undo)
decodeOrFailPureDB = decodeFull

dbGetBlundPure ::
       (HasConfiguration, MonadPureDB ctx m)
    => HeaderHash
    -> m (Maybe (Block, Undo))
dbGetBlundPure h = do
    (blund :: Maybe ByteString) <-
        view (pureBlocksStorage . at h) <$> (view (lensOf @DBPureVar) >>= readIORef)
    case decodeOrFailPureDB <$> blund of
        Nothing        -> pure Nothing
        Just (Left e)  -> throwM (DBMalformed e)
        Just (Right v) -> pure (Just v)

dbGetBlockPureDefault ::
       (HasConfiguration, MonadPureDB ctx m)
    => HeaderHash
    -> m (Maybe Block)
dbGetBlockPureDefault h = fmap fst <$> dbGetBlundPure h

dbGetUndoPureDefault ::
       forall ctx m. (HasConfiguration, MonadPureDB ctx m)
    => HeaderHash
    -> m (Maybe Undo)
dbGetUndoPureDefault h = fmap snd <$> dbGetBlundPure @ctx @m h

dbGetHeaderPureDefault ::
       (HasConfiguration, MonadDBRead m)
    => HeaderHash
    -> m (Maybe BlockHeader)
dbGetHeaderPureDefault = blkGetHeader

dbPutBlundPureDefault ::
       forall ctx m. (HasConfiguration, MonadPureDB ctx m)
    => Blund
    -> m ()
dbPutBlundPureDefault (blk,undo) = do
    let h = headerHash blk
    (var :: DBPureVar) <- view (lensOf @DBPureVar)
    flip atomicModifyIORefPure var $
        (pureBlocksStorage . at h .~ Just (serialize'  (blk,undo))) .
        (pureBlockIndexDB . at (blockIndexKey h) .~ Just (dbSerializeValue $ BC.getBlockHeader blk))

dbGetBlockSscPureDefault ::
       (HasConfiguration, MonadPureDB ctx m)
    => HeaderHash
    -> m (Maybe SscBlock)
dbGetBlockSscPureDefault = fmap (toSscBlock <$>) . dbGetBlockPureDefault

dbGetUndoSscPureDefault ::
       forall ctx m. (HasConfiguration, MonadPureDB ctx m)
    => HeaderHash
    -> m (Maybe ())
dbGetUndoSscPureDefault = fmap (const () <$>) . dbGetUndoPureDefault

dbGetHeaderSscPureDefault ::
       (HasConfiguration, MonadDBRead m)
    => HeaderHash
    -> m (Maybe (Some IsHeader))
dbGetHeaderSscPureDefault = fmap (Some <$>) . dbGetHeaderPureDefault

----------------------------------------------------------------------------
-- Rocks implementation
----------------------------------------------------------------------------

-- instance MonadBlockDBGeneric Block

type BlockDBGenericDefaultEnv ctx m =
    ( MonadDBRead m
    , MonadRealDB ctx m
    , HasConfiguration)

dbGetBlockDefault ::
       forall ctx m. (BlockDBGenericDefaultEnv ctx m)
    => HeaderHash
    -> m (Maybe Block)
dbGetBlockDefault = getBlock

dbGetUndoDefault ::
       forall ctx m. BlockDBGenericDefaultEnv ctx m
    => HeaderHash
    -> m (Maybe Undo)
dbGetUndoDefault = getUndo

dbGetHeaderDefault ::
       forall ctx m. (BlockDBGenericDefaultEnv ctx m)
    => HeaderHash
    -> m (Maybe BlockHeader)
dbGetHeaderDefault = blkGetHeader

dbGetBlockSscDefault ::
       forall ctx m. (BlockDBGenericDefaultEnv ctx m)
    => HeaderHash
    -> m (Maybe SscBlock)
dbGetBlockSscDefault = fmap (toSscBlock <$>) . getBlock

dbGetUndoSscDefault ::
       forall ctx m. BlockDBGenericDefaultEnv  ctx m
    => HeaderHash
    -> m (Maybe ())
dbGetUndoSscDefault = fmap (const () <$>) . getUndo

dbGetHeaderSscDefault ::
       forall ctx m. BlockDBGenericDefaultEnv ctx m
    => HeaderHash
    -> m (Maybe (Some IsHeader))
dbGetHeaderSscDefault = fmap (Some <$>) . blkGetHeader

-- instance MonadBlockDBWrite

dbPutBlundDefault :: (HasConfiguration, MonadDBRead m, MonadRealDB ctx m) => Blund -> m ()
dbPutBlundDefault = putBlundReal

----------------------------------------------------------------------------
-- DBSum implementation
----------------------------------------------------------------------------

type DBSumDefaultEnv ctx m =
    ( MonadDBRead m
    , MonadDBSum ctx m
    , HasConfiguration
    )

dbGetBlockSumDefault
    :: forall ctx m. (DBSumDefaultEnv ctx m)
    => HeaderHash -> m (Maybe Block)
dbGetBlockSumDefault hh = eitherDB (dbGetBlockDefault hh) (dbGetBlockPureDefault hh)

dbGetUndoSumDefault
    :: forall ctx m. DBSumDefaultEnv ctx m
    => HeaderHash -> m (Maybe Undo)
dbGetUndoSumDefault hh =
    eitherDB (dbGetUndoDefault hh) (dbGetUndoPureDefault hh)

dbGetHeaderSumDefault
    :: forall ctx m. (DBSumDefaultEnv ctx m)
    => HeaderHash -> m (Maybe BlockHeader)
dbGetHeaderSumDefault hh = eitherDB (dbGetHeaderDefault hh) (dbGetHeaderPureDefault hh)

-- instance MonadBlockDBGeneric

dbGetBlockSscSumDefault
    :: forall ctx m. (DBSumDefaultEnv ctx m)
    => HeaderHash -> m (Maybe SscBlock)
dbGetBlockSscSumDefault hh =
    eitherDB (dbGetBlockSscDefault hh) (dbGetBlockSscPureDefault hh)

dbGetUndoSscSumDefault
    :: forall ctx m. DBSumDefaultEnv ctx m
    => HeaderHash -> m (Maybe ())
dbGetUndoSscSumDefault hh =
    eitherDB (dbGetUndoSscDefault hh) (dbGetUndoSscPureDefault hh)

dbGetHeaderSscSumDefault
    :: forall ctx m. DBSumDefaultEnv ctx m
    => HeaderHash -> m (Maybe (Some IsHeader))
dbGetHeaderSscSumDefault hh =
    eitherDB (dbGetHeaderSscDefault hh) (dbGetHeaderSscPureDefault hh)

-- instance MonadBlockGenBase m

dbPutBlundSumDefault
    :: forall ctx m. (DBSumDefaultEnv ctx m)
    => Blund -> m ()
dbPutBlundSumDefault b = eitherDB (dbPutBlundDefault b) (dbPutBlundPureDefault b)

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

blkGetBlock ::
       (MonadBlockDB m)
    => HeaderHash
    -> m (Maybe Block)
blkGetBlock = dbGetBlock @BlockHeader @Block @Undo

blkGetUndo ::
       (MonadBlockDB m)
    => HeaderHash
    -> m (Maybe Undo)
blkGetUndo = dbGetUndo @BlockHeader @Block @Undo

blkGetBlund ::
       (MonadBlockDB m)
    => HeaderHash
    -> m (Maybe Blund)
blkGetBlund = dbGetBlund @BlockHeader @Block @Undo

putBi
    :: (MonadRealDB ctx m, Bi v)
    => ByteString -> v -> m ()
putBi k v = rocksPutBi k v =<< getBlockIndexDB

delete :: (MonadRealDB ctx m) => ByteString -> m ()
delete k = rocksDelete k =<< getBlockIndexDB

getData ::  forall m v . (MonadIO m, MonadCatch m, Bi v) => FilePath -> m (Maybe v)
getData fp = handle handler $ liftIO $
    either onDecodeError (pure . Just) . decodeFull =<< BS.readFile fp
  where
    onDecodeError :: Text -> IO a
    onDecodeError err =
        throwM $ DBMalformed $ sformat
        ("Couldn't deserialize "%build%", reason: "%build) fp err
    handler :: IOError -> m (Maybe x)
    handler e
        | isDoesNotExistError e = pure Nothing
        | otherwise = throwM e

putData ::  (MonadIO m, Bi v) => FilePath -> v -> m ()
putData fp v = liftIO $
    bracket (openBinaryFile fp WriteMode) hClose $ \h ->
        BS.hPut h (serialize' v) >> hFlush h

deleteData :: (MonadIO m, MonadCatch m) => FilePath -> m ()
deleteData fp = (liftIO $ removeFile fp) `catch` handler
  where
    handler e
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
    :: (MonadBlockDB m)
    => HeaderHash -> m Blund
getBlundThrow hash =
    maybeThrow (DBMalformed $ sformat errFmt hash) =<< blkGetBlund hash
  where
    errFmt = "getBlockThrow: no blund with HeaderHash: "%shortHashF

getBlockThrow
    :: (MonadBlockDB m)
    => HeaderHash -> m Block
getBlockThrow hash =
    maybeThrow (DBMalformed $ sformat errFmt hash) =<< blkGetBlock hash
  where
    errFmt = "getBlockThrow: no block with HeaderHash: "%shortHashF

getHeaderThrow
    :: (MonadDBRead m, HasConfiguration)
    => HeaderHash -> m BlockHeader
getHeaderThrow hash =
    maybeThrow (DBMalformed $ sformat errFmt hash) =<< blkGetHeader hash
  where
    errFmt = "getBlockThrow: no block header with hash: "%shortHashF
