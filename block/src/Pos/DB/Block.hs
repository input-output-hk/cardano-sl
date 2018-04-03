{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

-- | Interface and implementation of Blocks DB: storing blocks in files on disk.

module Pos.DB.Block
       ( getBlock
       , getUndo
       , getBlund
       , putBlunds
       , deleteBlock

       , getTipBlock

       , prepareBlockDB

       -- * Pure implementation
       , dbGetSerBlockPureDefault
       , dbGetSerUndoPureDefault
       , dbPutSerBlundsPureDefault

       -- * Rocks implementation
       , dbGetSerBlockRealDefault
       , dbGetSerUndoRealDefault
       , dbPutSerBlundsRealDefault

       -- * DBSum implementation
       , dbGetSerBlockSumDefault
       , dbGetSerUndoSumDefault
       , dbPutSerBlundsSumDefault
       ) where

import           Universum

import           Control.Exception.Safe (handle)
import           Control.Lens (at)
import qualified Data.ByteString as BS (hPut, readFile)
import           Data.Default (Default (def))
import           Formatting (formatToString)
import           System.Directory (createDirectoryIfMissing, removeFile)
import           System.FilePath ((</>))
import           System.IO (IOMode (WriteMode), hClose, hFlush, openBinaryFile)
import           System.IO.Error (IOError, isDoesNotExistError)

import           Pos.Binary.Block.Types ()
import           Pos.Binary.Class (Bi, decodeFull', serialize')
import           Pos.Binary.Core ()
import           Pos.Block.BHelpers ()
import           Pos.Block.Types (Blund, SerializedBlund, SlogUndo (..), Undo (..))
import           Pos.Core (HeaderHash, headerHash)
import           Pos.Core.Block (Block, GenesisBlock)
import qualified Pos.Core.Block as CB
import           Pos.Crypto (hashHexF)
import           Pos.DB.BlockIndex (deleteHeaderIndex, putHeadersIndex)
import           Pos.DB.Class (MonadDB (..), MonadDBRead (..), Serialized (..), SerializedBlock,
                               SerializedUndo, getBlock, getDeserialized)
import           Pos.DB.Error (DBError (..))
import           Pos.DB.GState.Common (getTipSomething)
import           Pos.DB.Pure (DBPureVar, MonadPureDB, atomicModifyIORefPure, pureBlocksStorage)
import           Pos.DB.Rocks (MonadRealDB, blockDataDir, getNodeDBs)
import           Pos.DB.Sum (MonadDBSum, eitherDB)
import           Pos.Delegation.Types (DlgUndo (..))
import           Pos.Util.Util (HasLens (..), eitherToThrow)

----------------------------------------------------------------------------
-- BlockDB related methods
----------------------------------------------------------------------------

getUndo :: MonadDBRead m => HeaderHash -> m (Maybe Undo)
getUndo = getDeserialized dbGetSerUndo

-- | Convenient wrapper which combines 'dbGetBlock' and 'dbGetUndo' to
-- read 'Blund'.
getBlund :: MonadDBRead m => HeaderHash -> m (Maybe (Block, Undo))
getBlund x =
    runMaybeT $
    (,) <$> MaybeT (getBlock x)
        <*> MaybeT (getUndo x)

putBlunds :: MonadDB m => NonEmpty Blund -> m ()
putBlunds = dbPutSerBlunds . map (fmap (Serialized . serialize'))

-- | Get 'Block' corresponding to tip.
getTipBlock :: MonadDBRead m => m Block
getTipBlock = getTipSomething "block" getBlock

----------------------------------------------------------------------------
-- Implementations for 'MonadRealDB'
----------------------------------------------------------------------------

-- Get serialization of a block with given hash from Block DB.
getSerializedBlock
    :: forall ctx m. (MonadRealDB ctx m)
    => HeaderHash -> m (Maybe ByteString)
getSerializedBlock = blockDataPath >=> getRawData

-- Get serialization of an undo data for block with given hash from Block DB.
getSerializedUndo :: (MonadRealDB ctx m) => HeaderHash -> m (Maybe ByteString)
getSerializedUndo = undoDataPath >=> getRawData

-- For every blund, put given block, its metadata and Undo data into
-- Block DB. This function uses 'MonadRealDB' constraint which is too
-- severe. Consider using 'dbPutBlund' instead.
putSerializedBlunds
    :: (MonadRealDB ctx m, MonadDB m)
    => NonEmpty SerializedBlund -> m ()
putSerializedBlunds (toList -> bs) = do
    bdd <- view blockDataDir <$> getNodeDBs
    let allData = map (\(b,u) -> let (dP, bP, uP) = getAllPaths bdd (headerHash b)
                                 in (dP,(b,u,bP,uP))
                      )
                      bs
    forM_ (ordNub $ map fst allData) $ \dPath ->
        liftIO $ createDirectoryIfMissing False dPath
    forM_ (map snd allData) $ \(blk,serUndo,bPath,uPath) -> do
        putData bPath blk
        putRawData uPath (unSerialized serUndo)
    putHeadersIndex $ toList $ map (CB.getBlockHeader . fst) bs

deleteBlock :: (MonadRealDB ctx m, MonadDB m) => HeaderHash -> m ()
deleteBlock hh = do
    deleteHeaderIndex hh
    bdd <- view blockDataDir <$> getNodeDBs
    let (_, bPath, uPath) = getAllPaths bdd hh
    deleteData bPath
    deleteData uPath

----------------------------------------------------------------------------
-- Initialization
----------------------------------------------------------------------------

prepareBlockDB
    :: MonadDB m
    => GenesisBlock -> m ()
prepareBlockDB blk =
    dbPutSerBlunds $ one (Left blk, Serialized $ serialize' genesisUndo)
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

decodeOrFailPureDB :: ByteString -> Either Text (Block, Undo)
decodeOrFailPureDB = decodeFull'

dbGetBlundPureDefault ::
       (MonadPureDB ctx m)
    => HeaderHash
    -> m (Maybe (Block, Undo))
dbGetBlundPureDefault h = do
    (blund :: Maybe ByteString) <-
        view (pureBlocksStorage . at h) <$> (view (lensOf @DBPureVar) >>= readIORef)
    case decodeOrFailPureDB <$> blund of
        Nothing        -> pure Nothing
        Just (Left e)  -> throwM (DBMalformed e)
        Just (Right v) -> pure (Just v)

dbGetSerBlockPureDefault
    :: (MonadPureDB ctx m)
    => HeaderHash
    -> m (Maybe SerializedBlock)
dbGetSerBlockPureDefault h = (Serialized . serialize' . fst) <<$>> dbGetBlundPureDefault h

dbGetSerUndoPureDefault
    :: forall ctx m. (MonadPureDB ctx m)
    => HeaderHash
    -> m (Maybe SerializedUndo)
dbGetSerUndoPureDefault h = (Serialized . serialize' . snd) <<$>> dbGetBlundPureDefault h

dbPutSerBlundsPureDefault ::
       forall ctx m. (MonadPureDB ctx m, MonadDB m)
    => NonEmpty SerializedBlund
    -> m ()
dbPutSerBlundsPureDefault (toList -> blunds) = do
    forM_ blunds $ \(blk, serUndo) -> do
        undo <- eitherToThrow $ first DBMalformed $ decodeFull' $ unSerialized serUndo
        let blund :: Blund -- explicit signature is required
            blund = (blk,undo)
        (var :: DBPureVar) <- view (lensOf @DBPureVar)
        flip atomicModifyIORefPure var $
            (pureBlocksStorage . at (headerHash blk) .~ Just (serialize' blund))
    putHeadersIndex $ map (CB.getBlockHeader . fst) blunds

----------------------------------------------------------------------------
-- Rocks implementation
----------------------------------------------------------------------------

-- instance MonadBlockDBGeneric Block

type BlockDBGenericEnv ctx m =
    ( MonadDBRead m
    , MonadRealDB ctx m
    )

dbGetSerBlockRealDefault ::
       forall ctx m. (BlockDBGenericEnv ctx m)
    => HeaderHash
    -> m (Maybe SerializedBlock)
dbGetSerBlockRealDefault x = Serialized <<$>> getSerializedBlock x

dbGetSerUndoRealDefault ::
       forall ctx m. BlockDBGenericEnv ctx m
    => HeaderHash
    -> m (Maybe SerializedUndo)
dbGetSerUndoRealDefault x = Serialized <<$>> getSerializedUndo x

dbPutSerBlundsRealDefault ::
       (MonadDB m, MonadRealDB ctx m)
    => NonEmpty SerializedBlund
    -> m ()
dbPutSerBlundsRealDefault = putSerializedBlunds

----------------------------------------------------------------------------
-- DBSum implementation
----------------------------------------------------------------------------

type DBSumEnv ctx m =
    ( MonadDB m
    , MonadDBSum ctx m
    )

dbGetSerBlockSumDefault
    :: forall ctx m. (DBSumEnv ctx m)
    => HeaderHash -> m (Maybe SerializedBlock)
dbGetSerBlockSumDefault hh = eitherDB (dbGetSerBlockRealDefault hh) (dbGetSerBlockPureDefault hh)

dbGetSerUndoSumDefault
    :: forall ctx m. DBSumEnv ctx m
    => HeaderHash -> m (Maybe SerializedUndo)
dbGetSerUndoSumDefault hh =
    eitherDB (dbGetSerUndoRealDefault hh) (dbGetSerUndoPureDefault hh)

dbPutSerBlundsSumDefault
    :: forall ctx m. (DBSumEnv ctx m)
    => NonEmpty SerializedBlund -> m ()
dbPutSerBlundsSumDefault b =
    eitherDB (dbPutSerBlundsRealDefault b) (dbPutSerBlundsPureDefault b)

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

getRawData ::  forall m . (MonadIO m, MonadCatch m) => FilePath -> m (Maybe ByteString)
getRawData  = handle handler . fmap Just . liftIO . BS.readFile
  where
    handler :: IOError -> m (Maybe x)
    handler e
        | isDoesNotExistError e = pure Nothing
        | otherwise = throwM e

putData ::  (MonadIO m, Bi v) => FilePath -> v -> m ()
putData fp = putRawData fp . serialize'

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

blockDataPath :: MonadRealDB ctx m => HeaderHash -> m FilePath
blockDataPath hh = do
    bdd <- view blockDataDir <$> getNodeDBs
    pure $ (view _2) $ getAllPaths bdd hh

undoDataPath :: MonadRealDB ctx m => HeaderHash -> m FilePath
undoDataPath hh = do
    bdd <- view blockDataDir <$> getNodeDBs
    pure $ (view _3) $ getAllPaths bdd hh

-- | Pass blockDataDir path
getAllPaths :: FilePath -> HeaderHash -> (FilePath, FilePath, FilePath)
getAllPaths bdd hh = (dir,bl,un)
  where
    (fn0, fn1) = splitAt 2 $ formatToString hashHexF hh
    dir = bdd </> fn0
    bl = dir </> (fn1 <> ".block")
    un = dir </> (fn1 <> ".undo")
