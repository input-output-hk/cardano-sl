{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

-- | Interface to Blocks DB.

module Pos.DB.Block
       ( getBlock
       , getUndo
       , getBlund
       , putBlund
       , deleteBlock

       , getTipBlock

       , prepareBlockDB

       -- * Pure implementation
       , dbGetRawBlockPureDefault
       , dbGetRawUndoPureDefault
       , dbPutRawBlundPureDefault

       -- * Rocks implementation
       , dbGetRawBlockRealDefault
       , dbGetRawUndoRealDefault
       , dbPutRawBlundRealDefault

       -- * DBSum implementation
       , dbGetRawBlockSumDefault
       , dbGetRawUndoSumDefault
       , dbPutRawBlundSumDefault
       ) where

import           Universum

import           Control.Exception.Safe (handle)
import           Control.Lens (at)
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
import           Pos.Block.BHelpers ()
import           Pos.Block.Types (Blund, RawBlund, SlogUndo (..), Undo (..))
import           Pos.Core (HasConfiguration, HeaderHash, headerHash)
import           Pos.Core.Block (Block, GenesisBlock)
import qualified Pos.Core.Block as CB
import           Pos.Crypto (hashHexF)
import           Pos.DB.BlockIndex (blockIndexKey)
import           Pos.DB.Class (MonadDB (..), MonadDBRead (..), getBlock)
import           Pos.DB.Error (DBError (..))
import           Pos.DB.Functions (dbSerializeValue)
import           Pos.DB.GState.Common (getTipSomething)
import           Pos.DB.Pure (DBPureVar, MonadPureDB, atomicModifyIORefPure, pureBlockIndexDB,
                              pureBlocksStorage)
import           Pos.DB.Rocks (MonadRealDB, blockDataDir, getBlockIndexDB, getNodeDBs, rocksDelete,
                               rocksPutBi)
import           Pos.DB.Sum (MonadDBSum, eitherDB)
import           Pos.Delegation.Types (DlgUndo (..))


----------------------------------------------------------------------------
-- BlockDB related methods
----------------------------------------------------------------------------

getUndo :: MonadDBRead m => HeaderHash -> m (Maybe Undo)
getUndo x = do
    mBS <- dbGetRawUndo x
    pure $ rightToMaybe . decodeFull =<< mBS

-- | Convenient wrapper which combines 'dbGetBlock' and 'dbGetUndo' to
-- read 'Blund'.
getBlund :: MonadDBRead m => HeaderHash -> m (Maybe (Block, Undo))
getBlund x =
    runMaybeT $
    (,) <$> MaybeT (getBlock x)
        <*> MaybeT (getUndo x)

putBlund :: MonadDB m => Blund -> m ()
putBlund = dbPutRawBlund . fmap serialize'

-- | Get 'Block' corresponding to tip.
getTipBlock :: MonadDBRead m => m Block
getTipBlock = getTipSomething "block" getBlock

----------------------------------------------------------------------------
-- Implementations for 'MonadRealDB'
----------------------------------------------------------------------------

-- Get block with given hash from Block DB.  This function has too
-- strict constraint, consider using 'blkGetBlock'.

getRawBlock
    :: forall ctx m. (HasConfiguration, MonadRealDB ctx m)
    => HeaderHash -> m (Maybe ByteString)
getRawBlock = blockDataPath >=> getData

-- Get undo data for block with given hash from Block DB. This
-- function has too strict constraint, consider using 'blkGetUndo'.
getRawUndo :: (HasConfiguration, MonadRealDB ctx m) => HeaderHash -> m (Maybe ByteString)
getRawUndo = undoDataPath >=> getData

-- Put given block, its metadata and Undo data into Block DB. This
-- function uses 'MonadRealDB' constraint which is too
-- severe. Consider using 'dbPutBlund' instead.
putRawBlund
    :: (HasConfiguration, MonadRealDB ctx m)
    => RawBlund -> m ()
putRawBlund (blk, rawUndo) = do
    let h = headerHash blk
    liftIO . createDirectoryIfMissing False =<< dirDataPath h
    flip putData blk =<< blockDataPath h
    flip putRawData rawUndo =<< undoDataPath h
    putBi (blockIndexKey h) (CB.getBlockHeader blk)

deleteBlock :: MonadRealDB ctx m => HeaderHash -> m ()
deleteBlock hh = do
    delete (blockIndexKey hh)
    deleteData =<< blockDataPath hh
    deleteData =<< undoDataPath hh

----------------------------------------------------------------------------
-- Initialization
----------------------------------------------------------------------------

prepareBlockDB
    :: MonadDB m
    => GenesisBlock -> m ()
prepareBlockDB blk =
    dbPutRawBlund (Left blk, serialize' genesisUndo)
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

dbGetBlundPureDefault ::
       (HasConfiguration, MonadPureDB ctx m)
    => HeaderHash
    -> m (Maybe (Block, Undo))
dbGetBlundPureDefault h = do
    (blund :: Maybe ByteString) <-
        view (pureBlocksStorage . at h) <$> (view (lensOf @DBPureVar) >>= readIORef)
    case decodeOrFailPureDB <$> blund of
        Nothing        -> pure Nothing
        Just (Left e)  -> throwM (DBMalformed e)
        Just (Right v) -> pure (Just v)

dbGetRawBlockPureDefault
    :: (HasConfiguration, MonadPureDB ctx m)
    => HeaderHash
    -> m (Maybe ByteString)
dbGetRawBlockPureDefault h = fmap (serialize' . fst) <$> dbGetBlundPureDefault h

dbGetRawUndoPureDefault
    :: forall ctx m. (HasConfiguration, MonadPureDB ctx m)
    => HeaderHash
    -> m (Maybe ByteString)
dbGetRawUndoPureDefault h = fmap (serialize' . snd) <$> dbGetBlundPureDefault @ctx @m h

dbPutRawBlundPureDefault ::
       forall ctx m. (HasConfiguration, MonadPureDB ctx m)
    => RawBlund
    -> m ()
dbPutRawBlundPureDefault (blk, rawUndo) = do
    let blund :: Blund
        blund =
            either (error "Couldn't deserialize undo")
                   (blk,)
                   (decodeFull rawUndo)
    let h = headerHash blk
    (var :: DBPureVar) <- view (lensOf @DBPureVar)
    flip atomicModifyIORefPure var $
        (pureBlocksStorage . at h .~ Just (serialize' blund)) .
        (pureBlockIndexDB . at (blockIndexKey h) .~ Just (dbSerializeValue $ CB.getBlockHeader blk))

----------------------------------------------------------------------------
-- Rocks implementation
----------------------------------------------------------------------------

-- instance MonadBlockDBGeneric Block

type BlockDBGenericEnv ctx m =
    ( MonadDBRead m
    , MonadRealDB ctx m
    , HasConfiguration)

dbGetRawBlockRealDefault ::
       forall ctx m. (BlockDBGenericEnv ctx m)
    => HeaderHash
    -> m (Maybe ByteString)
dbGetRawBlockRealDefault = getRawBlock

dbGetRawUndoRealDefault ::
       forall ctx m. BlockDBGenericEnv ctx m
    => HeaderHash
    -> m (Maybe ByteString)
dbGetRawUndoRealDefault = getRawUndo

dbPutRawBlundRealDefault :: (HasConfiguration, MonadDBRead m, MonadRealDB ctx m) => RawBlund -> m ()
dbPutRawBlundRealDefault = putRawBlund

----------------------------------------------------------------------------
-- DBSum implementation
----------------------------------------------------------------------------

type DBSumEnv ctx m =
    ( MonadDBRead m
    , MonadDBSum ctx m
    , HasConfiguration
    )

dbGetRawBlockSumDefault
    :: forall ctx m. (DBSumEnv ctx m)
    => HeaderHash -> m (Maybe ByteString)
dbGetRawBlockSumDefault hh = eitherDB (dbGetRawBlockRealDefault hh) (dbGetRawBlockPureDefault hh)

dbGetRawUndoSumDefault
    :: forall ctx m. DBSumEnv ctx m
    => HeaderHash -> m (Maybe ByteString)
dbGetRawUndoSumDefault hh =
    eitherDB (dbGetRawUndoRealDefault hh) (dbGetRawUndoPureDefault hh)

dbPutRawBlundSumDefault
    :: forall ctx m. (DBSumEnv ctx m)
    => RawBlund -> m ()
dbPutRawBlundSumDefault b = eitherDB (dbPutRawBlundRealDefault b) (dbPutRawBlundPureDefault b)

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

putBi
    :: (MonadRealDB ctx m, Bi v)
    => ByteString -> v -> m ()
putBi k v = rocksPutBi k v =<< getBlockIndexDB

delete :: (MonadRealDB ctx m) => ByteString -> m ()
delete k = rocksDelete k =<< getBlockIndexDB

getData ::  forall m v . (MonadIO m, MonadCatch m, Bi v) => FilePath -> m (Maybe v)
getData fp = handle handler $ liftIO $
    either onDecodeError (pure . Just) . decodeFull =<< getRawData fp
  where
    onDecodeError :: Text -> IO a
    onDecodeError err =
        throwM $ DBMalformed $ sformat
        ("Couldn't deserialize "%build%", reason: "%build) fp err
    handler :: IOError -> m (Maybe x)
    handler e
        | isDoesNotExistError e = pure Nothing
        | otherwise = throwM e

getRawData ::  forall m . MonadIO m => FilePath -> m ByteString
getRawData = liftIO . BS.readFile

putData ::  (MonadIO m, Bi v) => FilePath -> v -> m ()
putData fp v = liftIO $
    bracket (openBinaryFile fp WriteMode) hClose $ \h ->
        BS.hPut h (serialize' v) >> hFlush h

putRawData ::  MonadIO m => FilePath -> ByteString -> m ()
putRawData fp v = liftIO $
    bracket (openBinaryFile fp WriteMode) hClose $ \h ->
        BS.hPut h v >> hFlush h

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
