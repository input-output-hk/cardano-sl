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

       -- * Pure implementation
       , dbGetHeaderPure
       , dbGetBlockPure
       , dbGetUndoPure
       , dbGetBlockSscPure
       , dbGetUndoSscPure
       , dbGetHeaderSscPure
       , dbPutBlundPure

       -- * Rocks implementation
       , dbGetBlock
       , dbGetUndo
       , dbGetHeader
       , dbGetBlockSsc
       , dbGetUndoSsc
       , dbGetHeaderSsc
       , dbPutBlund

       -- * DBSum implementation
       , dbGetBlockSum
       , dbGetUndoSum
       , dbGetHeaderSum
       , dbGetBlockSscSum
       , dbGetUndoSscSum
       , dbGetHeaderSscSum
       , dbPutBlundSum
       ) where

import           Universum

import           Control.Exception.Safe (handle)
import           Control.Lens (at, _Wrapped)
import qualified Data.ByteString as BS (hPut, readFile)
import           Data.Default (Default (def))
import           Ether.Internal (HasLens (..))
import           Formatting (build, formatToString, sformat, (%))
import           System.Directory (createDirectoryIfMissing, removeFile)
import           System.FilePath ((</>))
import           System.IO (IOMode (WriteMode), hClose, hFlush, openBinaryFile)
import           System.IO.Error (IOError, isDoesNotExistError)

import           Pos.Binary.Block.Types ()
import           Pos.Binary.Class (Bi, decodeFull, serialize')
import           Pos.Binary.Core.Block ()
import           Pos.Block.Types (Blund, SlogUndo (..), Undo (..))
import           Pos.Core (BlockCount, HasConfiguration, HasDifficulty (difficultyL),
                           HasPrevBlock (prevBlockL), HeaderHash, IsHeader, headerHash)
import           Pos.Core.Block (Block, BlockHeader, GenesisBlock)
import qualified Pos.Core.Block as CB
import           Pos.Core.Configuration (genesisHash)
import           Pos.Crypto (hashHexF, shortHashF)
import           Pos.DB.Class (DBTag (..), MonadBlockDBGeneric (..), MonadBlockDBGenericWrite (..),
                               MonadDBRead, dbGetBlund)
import           Pos.DB.Error (DBError (..))
import           Pos.DB.Functions (dbGetBi, dbSerializeValue)
import           Pos.DB.Pure (DBPureVar, MonadPureDB, atomicModifyIORefPure, pureBlockIndexDB,
                              pureBlocksStorage)
import           Pos.DB.Rocks (MonadRealDB, blockDataDir, getBlockIndexDB, getNodeDBs, rocksDelete,
                               rocksPutBi)
import           Pos.DB.Sum (MonadDBSum, eitherDB)
import           Pos.Delegation.Types (DlgUndo (..))
import           Pos.Ssc (SscBlock)
import           Pos.Ssc.Util (toSscBlock)
import           Pos.Util (Some (..), maybeThrow)
import           Pos.Util.Chrono (NewestFirst (..))


----------------------------------------------------------------------------
-- BlockDB related methods
----------------------------------------------------------------------------

dbGetBlock :: MonadDBRead m => HeaderHash -> m (Maybe Block)
dbGetBlock x = undefined

dbGetUndo :: MonadDBRead m => HeaderHash -> m (Maybe Undo)
dbGetUndo x = undefined

-- | Convenient wrapper which combines 'dbGetBlock' and 'dbGetUndo' to
-- read 'Blund'.
dbGetBlund :: MonadDBRead m => HeaderHash -> m (Maybe (Block, Undo))
dbGetBlund x =
    runMaybeT $
    (,) <$> MaybeT (dbGetBlock x)
        <*> MaybeT (dbGetUndo x)

getBlundThrow
    :: MonadDBRead m
    => HeaderHash -> m Blund
getBlundThrow hash =
    maybeThrow (DBMalformed $ sformat errFmt hash) =<< dbGetBlund hash
  where
    errFmt = "getBlundThrow: no blund with HeaderHash: "%shortHashF

-- | Get 'Block' corresponding to tip.
getTipBlock :: MonadDBRead m => m Block
getTipBlock = getTipSomething "block" dbGetBlock

----------------------------------------------------------------------------
-- Implementations for 'MonadRealDB'
----------------------------------------------------------------------------

-- Get block with given hash from Block DB.  This function has too
-- strict constraint, consider using 'blkGetBlock'.

getBlock
    :: forall ctx m. (HasConfiguration, MonadRealDB ctx m)
    => HeaderHash -> m (Maybe Block)
getBlock = blockDataPath >=> getData

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
    putBi (blockIndexKey h) (CB.getBlockHeader blk)

deleteBlock :: (MonadRealDB ctx m) => HeaderHash -> m ()
deleteBlock hh = do
    delete (blockIndexKey hh)
    deleteData =<< blockDataPath hh
    deleteData =<< undoDataPath hh

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

dbGetBlockPure
    :: (HasConfiguration, MonadPureDB ctx m)
    => HeaderHash
    -> m (Maybe Block)
dbGetBlockPure h = fmap fst <$> dbGetBlundPure h

dbGetUndoPure
    :: forall ctx m. (HasConfiguration, MonadPureDB ctx m)
    => HeaderHash
    -> m (Maybe Undo)
dbGetUndoPure h = fmap snd <$> dbGetBlundPure @ctx @m h

dbGetHeaderPure
    :: (HasConfiguration, MonadDBRead m)
    => HeaderHash
    -> m (Maybe BlockHeader)
dbGetHeaderPure = blkGetHeader

dbPutBlundPure ::
       forall ctx m. (HasConfiguration, MonadPureDB ctx m)
    => Blund
    -> m ()
dbPutBlundPure (blk,undo) = do
    let h = headerHash blk
    (var :: DBPureVar) <- view (lensOf @DBPureVar)
    flip atomicModifyIORefPure var $
        (pureBlocksStorage . at h .~ Just (serialize'  (blk,undo))) .
        (pureBlockIndexDB . at (blockIndexKey h) .~ Just (dbSerializeValue $ CB.getBlockHeader blk))

dbGetBlockSscPure ::
       (HasConfiguration, MonadPureDB ctx m)
    => HeaderHash
    -> m (Maybe SscBlock)
dbGetBlockSscPure = fmap (toSscBlock <$>) . dbGetBlockPure

dbGetUndoSscPure ::
       forall ctx m. (HasConfiguration, MonadPureDB ctx m)
    => HeaderHash
    -> m (Maybe ())
dbGetUndoSscPure = fmap (const () <$>) . dbGetUndoPure

dbGetHeaderSscPure ::
       (HasConfiguration, MonadDBRead m)
    => HeaderHash
    -> m (Maybe (Some IsHeader))
dbGetHeaderSscPure = fmap (Some <$>) . dbGetHeaderPure

----------------------------------------------------------------------------
-- Rocks implementation
----------------------------------------------------------------------------

-- instance MonadBlockDBGeneric Block

type BlockDBGenericEnv ctx m =
    ( MonadDBRead m
    , MonadRealDB ctx m
    , HasConfiguration)

dbGetBlock ::
       forall ctx m. (BlockDBGenericEnv ctx m)
    => HeaderHash
    -> m (Maybe Block)
dbGetBlock = getBlock

dbGetUndo ::
       forall ctx m. BlockDBGenericEnv ctx m
    => HeaderHash
    -> m (Maybe Undo)
dbGetUndo = getUndo

dbGetHeader ::
       forall ctx m. (BlockDBGenericEnv ctx m)
    => HeaderHash
    -> m (Maybe BlockHeader)
dbGetHeader = blkGetHeader

dbGetBlockSsc ::
       forall ctx m. (BlockDBGenericEnv ctx m)
    => HeaderHash
    -> m (Maybe SscBlock)
dbGetBlockSsc = fmap (toSscBlock <$>) . getBlock

dbGetUndoSsc ::
       forall ctx m. BlockDBGenericEnv  ctx m
    => HeaderHash
    -> m (Maybe ())
dbGetUndoSsc = fmap (const () <$>) . getUndo

dbGetHeaderSsc ::
       forall ctx m. BlockDBGenericEnv ctx m
    => HeaderHash
    -> m (Maybe (Some IsHeader))
dbGetHeaderSsc = fmap (Some <$>) . blkGetHeader

-- instance MonadBlockDBWrite

dbPutBlund :: (HasConfiguration, MonadDBRead m, MonadRealDB ctx m) => Blund -> m ()
dbPutBlund = putBlundReal

----------------------------------------------------------------------------
-- DBSum implementation
----------------------------------------------------------------------------

type DBSumEnv ctx m =
    ( MonadDBRead m
    , MonadDBSum ctx m
    , HasConfiguration
    )

dbGetBlockSum
    :: forall ctx m. (DBSumEnv ctx m)
    => HeaderHash -> m (Maybe Block)
dbGetBlockSum hh = eitherDB (dbGetBlock hh) (dbGetBlockPure hh)

dbGetUndoSum
    :: forall ctx m. DBSumEnv ctx m
    => HeaderHash -> m (Maybe Undo)
dbGetUndoSum hh =
    eitherDB (dbGetUndo hh) (dbGetUndoPure hh)

dbGetHeaderSum
    :: forall ctx m. (DBSumEnv ctx m)
    => HeaderHash -> m (Maybe BlockHeader)
dbGetHeaderSum hh = eitherDB (dbGetHeader hh) (dbGetHeaderPure hh)

-- instance MonadBlockDBGeneric

dbGetBlockSscSum
    :: forall ctx m. (DBSumEnv ctx m)
    => HeaderHash -> m (Maybe SscBlock)
dbGetBlockSscSum hh =
    eitherDB (dbGetBlockSsc hh) (dbGetBlockSscPure hh)

dbGetUndoSscSum
    :: forall ctx m. DBSumEnv ctx m
    => HeaderHash -> m (Maybe ())
dbGetUndoSscSum hh =
    eitherDB (dbGetUndoSsc hh) (dbGetUndoSscPure hh)

dbGetHeaderSscSum
    :: forall ctx m. DBSumEnv ctx m
    => HeaderHash -> m (Maybe (Some IsHeader))
dbGetHeaderSscSum hh =
    eitherDB (dbGetHeaderSsc hh) (dbGetHeaderSscPure hh)

-- instance MonadBlockGenBase m

dbPutBlundSum
    :: forall ctx m. (DBSumEnv ctx m)
    => Blund -> m ()
dbPutBlundSum b = eitherDB (dbPutBlund b) (dbPutBlundPure b)

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

dbGetBlock ::
       (MonadBlockDB m)
    => HeaderHash
    -> m (Maybe Block)
dbGetBlock = dbGetBlock @BlockHeader @Block @Undo

dbGetUndo ::
       (MonadBlockDB m)
    => HeaderHash
    -> m (Maybe Undo)
dbGetUndo = dbGetUndo @BlockHeader @Block @Undo

blkGetBlund ::
       (MonadBlockDB m)
    => HeaderHash
    -> m (Maybe Blund)
blkGetBlund = dbGetBlund @BlockHeader @Block @Undo

-- | Convenient wrapper which combines 'dbGetBlock' and 'dbGetUndo' to
-- read 'Blund'.
dbGetBlund ::
       forall header blk undo m. MonadBlockDBGeneric header blk undo m
    => HeaderHash
    -> m $ Maybe (blk, undo)
dbGetBlund x =
    runMaybeT $
    (,) <$> MaybeT (dbGetBlock @header @blk @undo x) <*>
    MaybeT (dbGetUndo @header @blk @undo x)

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
