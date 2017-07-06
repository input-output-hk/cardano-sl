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

       -- * MonadBlockDB
       , MonadBlockDB
       , MonadBlockDBWrite (..)
       , BlockDBRedirect
       , runBlockDBRedirect
       ) where

import           Universum

import           Control.Lens                   (_Wrapped)
import           Control.Monad.Trans            (MonadTrans (..))
import           Control.Monad.Trans.Identity   (IdentityT (..))
import           Control.Monad.Trans.Lift.Local (LiftLocal (..))
import           Data.ByteArray                 (convert)
import qualified Data.ByteString                as BS (readFile, writeFile)
import qualified Data.ByteString.Lazy           as BSL
import           Data.Coerce                    (coerce)
import           Data.Default                   (Default (def))
import qualified Ether
import           Formatting                     (build, formatToString, sformat, (%))
import           System.Directory               (createDirectoryIfMissing, removeFile)
import           System.FilePath                ((</>))
import           System.IO.Error                (isDoesNotExistError)

import           Pos.Binary.Block               ()
import           Pos.Binary.Class               (Bi, decodeFull, encodeStrict)
import           Pos.Block.Core                 (Block, BlockHeader, GenesisBlock)
import qualified Pos.Block.Core                 as BC
import           Pos.Block.Types                (Blund, Undo (..))
import           Pos.Constants                  (genesisHash)
import           Pos.Core                       (HasDifficulty (difficultyL),
                                                 HasPrevBlock (prevBlockL), HeaderHash,
                                                 IsHeader, headerHash)
import           Pos.Crypto                     (hashHexF, shortHashF)
import           Pos.DB.Class                   (DBTag (..), MonadBlockDBGeneric (..),
                                                 MonadDBRead, MonadRealDB, dbGetBlund,
                                                 getBlockIndexDB, getNodeDBs)
import           Pos.DB.Error                   (DBError (DBMalformed))
import           Pos.DB.Functions               (dbGetBi, rocksDelete, rocksPutBi)
import           Pos.DB.Types                   (blockDataDir)
import           Pos.Ssc.Class.Helpers          (SscHelpersClass)
import           Pos.Ssc.Class.Types            (SscBlock)
import           Pos.Ssc.Util                   (toSscBlock)
import           Pos.Util                       (Some (..), maybeThrow)
import           Pos.Util.Chrono                (NewestFirst (..))

----------------------------------------------------------------------------
-- Implementations for 'MonadRealDB'
----------------------------------------------------------------------------

-- Get block with given hash from Block DB.  This function has too
-- strict constraint, consider using 'blkGetBlock'.
getBlock
    :: forall ssc m. (SscHelpersClass ssc, MonadRealDB m)
    => HeaderHash -> m (Maybe (Block ssc))
getBlock = blockDataPath >=> getData

-- | Returns header of block that was requested from Block DB.
blkGetHeader
    :: (SscHelpersClass ssc, MonadDBRead m)
    => HeaderHash -> m (Maybe (BlockHeader ssc))
blkGetHeader = dbGetBi BlockIndexDB . blockIndexKey

-- Get undo data for block with given hash from Block DB. This
-- function has too strict constraint, consider using 'blkGetUndo'.
getUndo :: (MonadRealDB m) => HeaderHash -> m (Maybe Undo)
getUndo = undoDataPath >=> getData

-- Put given block, its metadata and Undo data into Block DB. This
-- function uses 'MonadRealDB' constraint which is too
-- severe. Consider using 'dbPutBlund' instead.
putBlundReal
    :: (SscHelpersClass ssc, MonadRealDB m)
    => Blund ssc -> m ()
putBlundReal (blk, undo) = do
    let h = headerHash blk
    liftIO . createDirectoryIfMissing False =<< dirDataPath h
    flip putData blk =<< blockDataPath h
    flip putData undo =<< undoDataPath h
    putBi (blockIndexKey h) (BC.getBlockHeader blk)

deleteBlock :: (MonadRealDB m) => HeaderHash -> m ()
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
    :: (MonadBlockDB ssc m)
    => (Block ssc -> Bool) -> HeaderHash -> m (NewestFirst [] (Blund ssc))
loadBlundsWhile predicate = loadDataWhile getBlundThrow (predicate . fst)

-- | Load blunds which have depth less than given (depth = number of
-- blocks that will be returned).
loadBlundsByDepth
    :: (MonadBlockDB ssc m)
    => Word -> HeaderHash -> m (NewestFirst [] (Blund ssc))
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
    => Word -> HeaderHash -> m (NewestFirst [] (BlockHeader ssc))
loadHeadersByDepth = loadDataByDepth getHeaderThrow (const True)

-- | Load headers which have depth less than given and match some criterion.
loadHeadersByDepthWhile
    :: (SscHelpersClass ssc, MonadDBRead m)
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
       MonadBlockDBWrite ssc m
    => GenesisBlock ssc -> m ()
prepareBlockDB blk = dbPutBlund (Left blk, def)

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

data BlockDBRedirectTag

type BlockDBRedirect =
    Ether.TaggedTrans BlockDBRedirectTag IdentityT

runBlockDBRedirect :: BlockDBRedirect m a -> m a
runBlockDBRedirect = coerce

-- instance MonadBlockDBGeneric (Block ssc)

instance (MonadDBRead m, MonadRealDB m, t ~ IdentityT, SscHelpersClass ssc) =>
         MonadBlockDBGeneric (BlockHeader ssc) (Block ssc) Undo (Ether.TaggedTrans BlockDBRedirectTag t m) where
    dbGetBlock  = getBlock
    dbGetUndo   = getUndo
    dbGetHeader = blkGetHeader

-- instance MonadBlockDBGeneric (SscBlock ssc)

instance (MonadDBRead m, MonadRealDB m, t ~ IdentityT, SscHelpersClass ssc) =>
         MonadBlockDBGeneric (Some IsHeader) (SscBlock ssc) () (Ether.TaggedTrans BlockDBRedirectTag t m) where
    dbGetBlock  = fmap (toSscBlock <$>) . getBlock
    dbGetUndo   = fmap (const () <$>)   . getUndo
    dbGetHeader = fmap (Some <$>)       . blkGetHeader @ssc

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

-- modifications

-- | Superclass of 'MonadBlockDB' which allows to modify the Block
-- DB. It's defined here instead of `cardano-sl-db`, because it makes
-- sense to use it only in block processing component.
--
-- TODO: support deletion when we actually start using deletion
-- (probably not soon).
class MonadBlockDB ssc m => MonadBlockDBWrite ssc m where
    -- | Put given 'Blund' into the Block DB.
    dbPutBlund :: Blund ssc -> m ()

instance {-# OVERLAPPABLE #-}
    (MonadBlockDBWrite ssc m, MonadTrans t, LiftLocal t, MonadThrow (t m)) =>
        MonadBlockDBWrite ssc (t m)
  where
    dbPutBlund = lift . dbPutBlund

-- instance MonadBlockDBWrite

instance (MonadDBRead m, MonadRealDB m, t ~ IdentityT, SscHelpersClass ssc) =>
         MonadBlockDBWrite ssc (Ether.TaggedTrans BlockDBRedirectTag t m) where
    dbPutBlund = putBlundReal

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

putBi
    :: (MonadRealDB m, Bi v)
    => ByteString -> v -> m ()
putBi k v = rocksPutBi k v =<< getBlockIndexDB

delete :: (MonadRealDB m) => ByteString -> m ()
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

dirDataPath :: MonadRealDB m => HeaderHash -> m FilePath
dirDataPath (formatToString hashHexF -> fn) = gitDirDataPath fn

blockDataPath :: MonadRealDB m => HeaderHash -> m FilePath
blockDataPath (formatToString (hashHexF%".block") -> fn) =
    gitDirDataPath fn <&> (</> drop 2 fn)

undoDataPath :: MonadRealDB m => HeaderHash -> m FilePath
undoDataPath (formatToString (hashHexF%".undo") -> fn) =
    gitDirDataPath fn <&> (</> drop 2 fn)

gitDirDataPath :: MonadRealDB m => [Char] -> m FilePath
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
