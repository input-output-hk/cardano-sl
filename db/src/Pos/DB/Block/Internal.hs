{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

-- | Interface and implementation of Blocks DB: storing blocks in files on disk.

module Pos.DB.Block.Internal
       ( BlockStoragePaths (..)

       , dbGetSerBlundRealFile
       , dbGetSerBlundPureDefault
       , dbGetSerBlockPureDefault
       , dbGetSerUndoPureDefault
       , dbPutSerBlundsPureDefault
       , dbGetSerBlockRealFile
       , dbGetSerUndoRealFile
       , dbPutSerBlundsRealDefault

       , deleteBlock
       , getSerializedBlund
       , prepareBlockDB

       , getAllPaths
       , getRawData
       , putRawData
       , deleteData
       ) where

import           Universum

import           Control.Exception.Safe (handle)
import           Control.Lens (at)
import qualified Data.ByteString as BS
import           Data.Default (Default (def))
import           Formatting (formatToString)
import           System.Directory (createDirectoryIfMissing, doesFileExist,
                     removeFile)
import           System.FilePath ((</>))
import           System.IO (IOMode (WriteMode), hClose, hFlush, openBinaryFile)
import           System.IO.Error (IOError, isDoesNotExistError)

import           Pos.Binary.Class (decodeFull', serialize')
import           Pos.Chain.Block (Block, GenesisBlock, HeaderHash,
                     SlogUndo (..), Undo (..), headerHash)
import qualified Pos.Chain.Block as CB
import           Pos.Chain.Delegation (DlgUndo (..))
import           Pos.Crypto (hashHexF)
import           Pos.DB.BlockIndex (deleteHeaderIndex, putHeadersIndex)
import           Pos.DB.Class (MonadDB (..), MonadDBRead (..), Serialized (..),
                     SerializedBlock, SerializedBlund, SerializedUndo)
import           Pos.DB.Error (DBError (..))
import           Pos.DB.Pure (DBPureVar, MonadPureDB, atomicModifyIORefPure,
                     pureBlocksStorage)
import           Pos.DB.Rocks (MonadRealDB, blockDataDir, getNodeDBs)
import           Pos.Util.Util (HasLens (..), eitherToThrow)


----------------------------------------------------------------------------
-- Implementations for 'MonadRealDB'
----------------------------------------------------------------------------

-- Get serialization of a block with given hash from Block DB.
getSerializedBlock
    :: MonadRealDB ctx m
    => HeaderHash -> m (Maybe SerializedBlock)
getSerializedBlock hh =
    Serialized . fst <<$>> getSerializedBlund hh

-- Get serialization of an undo data for block with given hash from Block DB.
getSerializedUndo
    :: MonadRealDB ctx m
    => HeaderHash -> m (Maybe SerializedUndo)
getSerializedUndo hh =
    Serialized . snd <<$>> getSerializedBlund hh

-- Get serialization of a block with given hash from Block DB.
getSerializedBlund
    :: MonadRealDB ctx m
    => HeaderHash -> m (Maybe (ByteString, ByteString))
getSerializedBlund hh = do
    bsp <- flip getAllPaths hh . view blockDataDir <$> getNodeDBs
    blundExists <- liftIO $ doesFileExist (bspBlund bsp)
    if blundExists
    then do
      mbs <- getRawData $ bspBlund bsp
      case mbs of
        Nothing -> pure Nothing
        Just ser -> eitherToThrow . bimap DBMalformed Just
                    $ decodeFull' @(ByteString, ByteString) ser
    else consolidateBlund hh

-- | Read independent block and undo data and consolidate them into a single
-- blund file.
consolidateBlund
    :: MonadRealDB ctx m
    => HeaderHash
    -> m (Maybe (ByteString, ByteString))
consolidateBlund hh = do
    bsp <- flip getAllPaths hh . view blockDataDir <$> getNodeDBs
    block <- getRawData $ bspBlock bsp
    undo <- getRawData $ bspUndo bsp
    forM ((,) <$> block <*> undo) $ \blund -> do
        putRawData (bspBlund bsp) $ serialize' blund
        liftIO . removeFile $ bspBlock bsp
        liftIO . removeFile $ bspUndo bsp
        return blund


-- For every blund, put given block, its metadata and Undo data into Block DB.
--
-- TODO What does this comment mean? If the constraint isn't needed, why is it
-- here? The referenced 'dbPutBlund' function doesn't even exist.
--
-- This function uses 'MonadRealDB' constraint which is too severe.
-- Consider using 'dbPutBlund' instead.
putSerializedBlunds
    :: (MonadRealDB ctx m, MonadDB m)
    => NonEmpty (CB.BlockHeader, SerializedBlund) -> m ()
putSerializedBlunds (toList -> bs) = do
    bdd <- view blockDataDir <$> getNodeDBs
    let allData = map (\(bh,bu) -> let bsp = getAllPaths bdd (headerHash bh)
                                    in (bspRoot bsp,(bu, bspBlund bsp))
                      )
                      bs
    forM_ (ordNub $ map fst allData) $ \dPath ->
        liftIO $ createDirectoryIfMissing False dPath
    forM_ (map snd allData) $ \(blund,buPath) -> do
        putRawData buPath $ unSerialized blund
    putHeadersIndex $ toList $ map fst bs

deleteBlock :: (MonadRealDB ctx m, MonadDB m) => HeaderHash -> m ()
deleteBlock hh = do
    deleteHeaderIndex hh
    bdd <- view blockDataDir <$> getNodeDBs
    let bsp = getAllPaths bdd hh
    mapM_ deleteData [bspBlock bsp, bspUndo bsp, bspBlund bsp]

----------------------------------------------------------------------------
-- Initialization
----------------------------------------------------------------------------

prepareBlockDB
    :: MonadDB m
    => GenesisBlock -> m ()
prepareBlockDB blk =
    dbPutSerBlunds
    $ one ( CB.getBlockHeader $ Left blk
          , Serialized . serialize' $ bimap (serialize' @Block) serialize' (Left blk, genesisUndo))
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

dbGetSerBlockPureDefault
    :: (MonadPureDB ctx m)
    => HeaderHash
    -> m (Maybe SerializedBlock)
dbGetSerBlockPureDefault h = do
    (serblund :: Maybe ByteString) <-
        view (pureBlocksStorage . at h) <$> (view (lensOf @DBPureVar) >>= readIORef)
    case decodeFull' @(ByteString, ByteString) <$> serblund of
        Nothing        -> pure Nothing
        Just (Left e)  -> throwM (DBMalformed e)
        Just (Right v) -> pure . Just . Serialized $ fst v

dbGetSerUndoPureDefault
    :: forall ctx m. (MonadPureDB ctx m)
    => HeaderHash
    -> m (Maybe SerializedUndo)
dbGetSerUndoPureDefault h =  do
    (serblund :: Maybe ByteString) <-
        view (pureBlocksStorage . at h) <$> (view (lensOf @DBPureVar) >>= readIORef)
    case decodeFull' @(ByteString, ByteString) <$> serblund of
        Nothing        -> pure Nothing
        Just (Left e)  -> throwM (DBMalformed e)
        Just (Right v) -> pure . Just . Serialized $ snd v

dbGetSerBlundPureDefault
    :: forall ctx m. (MonadPureDB ctx m)
    => HeaderHash
    -> m (Maybe SerializedBlund)
dbGetSerBlundPureDefault h =  do
    (serblund :: Maybe ByteString) <-
        view (pureBlocksStorage . at h) <$> (view (lensOf @DBPureVar) >>= readIORef)
    case decodeFull' @(ByteString, ByteString) <$> serblund of
        Nothing        -> pure Nothing
        Just (Left e)  -> throwM (DBMalformed e)
        Just (Right v) -> pure . Just . Serialized $ uncurry BS.append v

dbPutSerBlundsPureDefault ::
       forall ctx m. (MonadPureDB ctx m, MonadDB m)
    => NonEmpty (CB.BlockHeader, SerializedBlund)
    -> m ()
dbPutSerBlundsPureDefault (toList -> blunds) = do
    forM_ blunds $ \(bh, serBlund) -> do
        (var :: DBPureVar) <- view (lensOf @DBPureVar)
        flip atomicModifyIORefPure var $
            (pureBlocksStorage . at (headerHash bh) .~ Just (unSerialized serBlund))
    putHeadersIndex $ map fst blunds

----------------------------------------------------------------------------
-- Rocks implementation
----------------------------------------------------------------------------

type BlockDBGenericEnv ctx m =
    ( MonadDBRead m
    , MonadRealDB ctx m
    )

-- The following 'dbGetSer*RealFile' functions get Blocks/Blunds/Undos when
-- they are stored in files. They will fail (return 'Nothing') for items when
-- those items are part of a epoch that has been consolidated into a single
-- file. That means these functions should not be called directly from outside
-- the DB package.

dbGetSerBlockRealFile ::
       forall ctx m. (BlockDBGenericEnv ctx m)
    => HeaderHash
    -> m (Maybe SerializedBlock)
dbGetSerBlockRealFile = getSerializedBlock

dbGetSerUndoRealFile ::
       forall ctx m. BlockDBGenericEnv ctx m
    => HeaderHash
    -> m (Maybe SerializedUndo)
dbGetSerUndoRealFile = getSerializedUndo

dbGetSerBlundRealFile ::
       forall ctx m. (BlockDBGenericEnv ctx m)
    => HeaderHash
    -> m (Maybe SerializedBlund)
dbGetSerBlundRealFile hh =
    Serialized . uncurry BS.append <<$>> getSerializedBlund hh

dbPutSerBlundsRealDefault ::
       (MonadDB m, MonadRealDB ctx m)
    => NonEmpty (CB.BlockHeader, SerializedBlund)
    -> m ()
dbPutSerBlundsRealDefault = putSerializedBlunds

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

-- | Paths at which we store the block data.
data BlockStoragePaths = BlockStoragePaths
    { bspRoot  :: FilePath
      -- | Block data itself.
    , bspBlock :: FilePath
      -- | Undo information for a block.
    , bspUndo  :: FilePath
      -- | Combined storage format. Either this or a combination of 'Block' and
      -- 'Undo' files will be present.
    , bspBlund :: FilePath
    }

-- | Pass blockDataDir path
getAllPaths :: FilePath -> HeaderHash -> BlockStoragePaths
getAllPaths bdd hh = BlockStoragePaths dir bl un blund
  where
    (fn0, fn1) = splitAt 2 $ formatToString hashHexF hh
    dir = bdd </> fn0
    bl = dir </> (fn1 <> ".block")
    un = dir </> (fn1 <> ".undo")
    blund = dir </> (fn1 <> ".blund")
