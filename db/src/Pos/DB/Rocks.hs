{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}

module Pos.DB.Rocks
    ( -- * Opening and modifications
      openRocksDB
    , closeRocksDB
    , openNodeDBs
    , closeNodeDBs
    , deleteNodeDBs
    , usingReadOptions
    , usingWriteOptions

    -- * Reading/writing
    , rocksGetBytes
    , rocksPutBytes
    , rocksDelete
    , rocksPutBi

    -- * Iteration
    , rocksIterSource

    -- * Default methods
    , dbGetDefault
    , dbIterSourceDefault
    , dbPutDefault
    , dbWriteBatchDefault
    , dbDeleteDefault
    ) where

-- import           Pos.DB.Rocks.Functions
import           Universum

import           Control.Lens (makeLenses)
import qualified Database.RocksDB as Rocks
import           Control.Lens (ASetter')
import           Control.Monad.Trans.Resource (MonadResource)
import           Data.Conduit (ConduitT, bracketP, yield)
import qualified Database.RocksDB as Rocks
import           System.Directory (createDirectoryIfMissing, doesDirectoryExist,
                     removeDirectoryRecursive)
import           System.FilePath (takeDirectory, (</>))
import           Control.Monad.Trans (MonadTrans (..))
import           Control.Monad.Trans.Resource (ResourceT, transResourceT)
import           Data.Conduit (ConduitT, transPipe)
import qualified Database.RocksDB as Rocks
import           Serokell.Data.Memory.Units (Byte)
import qualified Database.RocksDB as Rocks
import           Formatting (bprint)
import qualified Formatting.Buildable
import           Serokell.Util.Text (listJson)
import           Control.Lens (ASetter')
import           Control.Monad.Trans.Resource (MonadResource)
import           Data.Conduit (ConduitT, bracketP, yield)
import qualified Database.RocksDB as Rocks
import           System.Directory (createDirectoryIfMissing, doesDirectoryExist,
                     removeDirectoryRecursive)
import           System.FilePath (takeDirectory, (</>))
import           Control.Monad.Trans (MonadTrans (..))
import           Control.Monad.Trans.Resource (ResourceT, transResourceT)
import           Data.Conduit (ConduitT, transPipe)
import qualified Database.RocksDB as Rocks
import           Serokell.Data.Memory.Units (Byte)


import           Pos.Binary.Class (Bi, serialize')
-- import           Pos.DB.BatchOp (rocksWriteBatch)
import qualified Pos.Util.Concurrent.RWLock as RWL
import           Pos.Util.Util (lensOf)
import           Pos.Binary.Class (Bi, decodeFull')
import           Pos.Chain.Block (Block, BlockHeader, HeaderHash)
import           Pos.Chain.Genesis (GenesisHash)
import           Pos.Chain.Update (BlockVersionData (..))
import           Pos.Core (EpochIndex, isBootstrapEra)
import           Pos.DB.Error (DBError (DBMalformed))
import           Pos.Util.Util (eitherToThrow, HasLens')
import           Pos.DB.Rocks (DB (..))
import           Pos.Binary.Class (Bi, serialize')
import           Pos.DB.Rocks.Types (DB (..), MonadRealDB, NodeDBs (..),
                     getDBByTag, DBTag(..), DBIteratorClass, IterType, IterKey,
                     IterValue)
import qualified Pos.Util.Concurrent.RWLock as RWL
import           Pos.Util.Util (lensOf)
import Pos.DB.Block
import           Pos.Binary.Class (Bi, decodeFull')
import           Pos.Chain.Block (Block, BlockHeader, HeaderHash)
import           Pos.Chain.Genesis (GenesisHash)
import           Pos.Chain.Update (BlockVersionData (..))
import           Pos.Core (EpochIndex, isBootstrapEra)
import           Pos.DB.Error (DBError (DBMalformed))
import           Pos.Util.Util (eitherToThrow, HasLens')

import           Pos.Util.Concurrent.RWLock (RWLock)
import           Pos.Util.Util (HasLens (..))


----------------------------------------------------------------------------
-- Opening/options
----------------------------------------------------------------------------

openRocksDB :: MonadIO m => FilePath -> m DB
openRocksDB fp = DB Rocks.defaultReadOptions Rocks.defaultWriteOptions options
                   <$> Rocks.open options
  where options = (Rocks.defaultOptions fp)
          { Rocks.optionsCreateIfMissing = True
          , Rocks.optionsCompression     = Rocks.NoCompression
          }

closeRocksDB :: MonadIO m => DB -> m ()
closeRocksDB = Rocks.close . rocksDB

-- | Open all DBs stored on disk.
-- Don't forget to use 'closeNodeDBs' eventually.
openNodeDBs
    :: (MonadIO m)
    => Bool -> FilePath -> m NodeDBs
openNodeDBs recreate fp = do
    liftIO $
        whenM ((recreate &&) <$> doesDirectoryExist fp) $
        removeDirectoryRecursive fp
    let blocksDir = fp </> "blocks"
    let blocksIndexPath = blocksDir </> "index"
    let _blockDataDir = blocksDir </> "data"
    let _epochDataDir = fp </> "epochs"
    let gStatePath = fp </> "gState"
    let lrcPath = fp </> "lrc"
    let miscPath = fp </> "misc"
    mapM_ ensureDirectoryExists
        [ blocksDir
        , _blockDataDir
        , blocksIndexPath
        , _epochDataDir
        , gStatePath
        , lrcPath
        , miscPath
        ]
    _blockIndexDB <- openRocksDB blocksIndexPath
    _gStateDB <- openRocksDB gStatePath
    _lrcDB <- openRocksDB lrcPath
    _miscDB <- openRocksDB miscPath
    _epochLock <- RWL.new
    pure NodeDBs {..}
  where
    ensureDirectoryExists :: MonadIO m => FilePath -> m ()
    ensureDirectoryExists = liftIO . createDirectoryIfMissing True

-- | Safely close all databases from 'NodeDBs'.
closeNodeDBs :: MonadIO m => NodeDBs -> m ()
closeNodeDBs NodeDBs {..} =
    mapM_ closeRocksDB [_blockIndexDB, _gStateDB, _lrcDB, _miscDB]

deleteNodeDBs :: MonadIO m => NodeDBs -> m ()
deleteNodeDBs =
    liftIO . removeDirectoryRecursive . takeDirectory . _epochDataDir

usingReadOptions
    :: MonadRealDB ctx m
    => Rocks.ReadOptions
    -> ASetter' NodeDBs DB
    -> m a
    -> m a
usingReadOptions opts l =
    local (over (lensOf @NodeDBs . l) (\db -> db {rocksReadOpts = opts}))

usingWriteOptions
    :: MonadRealDB ctx m
    => Rocks.WriteOptions
    -> ASetter' NodeDBs DB
    -> m a
    -> m a
usingWriteOptions opts l =
    local (over (lensOf @NodeDBs . l) (\db -> db {rocksWriteOpts = opts}))

----------------------------------------------------------------------------
-- Reading/writing
----------------------------------------------------------------------------

-- | Read ByteString from RocksDb using given key.
rocksGetBytes :: (MonadIO m) => ByteString -> DB -> m (Maybe ByteString)
rocksGetBytes key DB {..} = Rocks.get rocksDB rocksReadOpts key

-- | Write ByteString to RocksDB for given key.
rocksPutBytes :: (MonadIO m) => ByteString -> ByteString -> DB -> m ()
rocksPutBytes k v DB {..} = Rocks.put rocksDB rocksWriteOpts k v

-- | Delete element from RocksDB for given key.
rocksDelete :: (MonadIO m) => ByteString -> DB -> m ()
rocksDelete k DB {..} = Rocks.delete rocksDB rocksWriteOpts k

-- garbage, should be abstracted and hidden

-- | Write serializable value to RocksDb for given key.
rocksPutBi :: (Bi v, MonadIO m) => ByteString -> v -> DB -> m ()
rocksPutBi k v = rocksPutBytes k (serialize' v)

----------------------------------------------------------------------------
-- Snapshot
----------------------------------------------------------------------------

-- The following is not used in the project yet. So the
-- rocksdb-haskell-ng binding doesn't include it yet, and we simply
-- comment it out here, to be added back at a later stage when needed.

{-
newtype Snapshot = Snapshot Rocks.Snapshot

usingSnapshot
    :: (MonadIO m, MonadMask m)
    => DB -> (Snapshot -> m a) -> m a
usingSnapshot DB {..} action =
    bracket
        (Rocks.createSnapshot rocksDB)
        (Rocks.releaseSnapshot rocksDB)
        (action . Snapshot)
-}

----------------------------------------------------------------------------
-- Iteration
----------------------------------------------------------------------------

-- | Conduit source built from rocks iterator.
rocksIterSource ::
       forall ctx m i.
       ( MonadResource m
       , MonadRealDB ctx m
       , DBIteratorClass i
       , Bi (IterKey i)
       , Bi (IterValue i)
       )
    => DBTag
    -> Proxy i
    -> ConduitT () (IterType i) m ()
rocksIterSource tag _ = do
    DB{..} <- lift $ getDBByTag tag
    let createIter = Rocks.createIter rocksDB rocksReadOpts
    let releaseIter i = Rocks.releaseIter i
    let action iter = do
            Rocks.iterSeek iter (iterKeyPrefix @i)
            produce iter
    bracketP createIter releaseIter action
  where
    produce :: Rocks.Iterator -> ConduitT () (IterType i) m ()
    produce it = do
        entryStr <- processRes =<< Rocks.iterEntry it
        case entryStr of
            Nothing -> pass
            Just e -> do
                yield e
                Rocks.iterNext it
                produce it
    processRes ::
           Maybe (ByteString, ByteString)
        -> ConduitT () (IterType i) m (Maybe (IterType i))
    processRes Nothing   = pure Nothing
    processRes (Just kv) = processIterEntry @i kv

----------------------------------------------------------------------------
-- Implementation/default methods
----------------------------------------------------------------------------

dbGetDefault :: MonadRealDB ctx m => DBTag -> ByteString -> m (Maybe ByteString)
dbGetDefault tag key = getDBByTag tag >>= rocksGetBytes key

dbPutDefault :: MonadRealDB ctx m => DBTag -> ByteString -> ByteString -> m ()
dbPutDefault tag key val = getDBByTag tag >>= rocksPutBytes key val

dbWriteBatchDefault :: MonadRealDB ctx m => DBTag -> [Rocks.BatchOp] -> m ()
dbWriteBatchDefault tag batch = getDBByTag tag >>= rocksWriteBatch batch

dbDeleteDefault :: MonadRealDB ctx m => DBTag -> ByteString -> m ()
dbDeleteDefault tag key = getDBByTag tag >>= rocksDelete key

dbIterSourceDefault ::
       forall ctx m i.
       ( MonadRealDB ctx m
       , MonadResource m
       , DBIteratorClass i
       , Bi (IterKey i)
       , Bi (IterValue i)
       )
    => DBTag
    -> Proxy i
    -> ConduitT () (IterType i) m ()
dbIterSourceDefault = rocksIterSource

-- | Write a batch of some operations to RocksDB.
rocksWriteBatch :: (RocksBatchOp a, MonadIO m) => [a] -> DB -> m ()
rocksWriteBatch batch DB {..} =
    Rocks.write rocksDB rocksWriteOpts (concatMap toBatchOp batch)

-- import           Pos.DB.Rocks.Types

-- | Key-value type family encapsulating the iterator (something we
-- can iterate on) functionality.
class DBIteratorClass i where
    type IterKey   i :: *
    type IterValue i :: *
    iterKeyPrefix :: ByteString

-- | Tag which denotes one of DBs used by application.
data DBTag
    = BlockIndexDB
    | GStateDB
    | LrcDB
    | MiscDB
    deriving (Eq)

type IterType i = (IterKey i, IterValue i)

-- | This is the set of constraints necessary to operate on «real» DBs
-- (which are wrapped into 'NodeDBs').  Apart from providing access to
-- 'NodeDBs' it also has 'MonadIO' constraint, because it's impossible
-- to use real DB without IO. Finally, it has 'MonadCatch' constraints
-- (partially for historical reasons, partially for good ones).
type MonadRealDB ctx m =
    ( MonadReader ctx m
    , HasLens NodeDBs ctx NodeDBs
    , MonadIO m
    , MonadCatch m
    )

-- should we replace `rocks` prefix by other or remove it at all?
data DB = DB
    { rocksReadOpts  :: !Rocks.ReadOptions
    , rocksWriteOpts :: !Rocks.WriteOptions
    , rocksOptions   :: !Rocks.Options
    , rocksDB        :: !Rocks.DB
    }

data NodeDBs = NodeDBs
    { _blockIndexDB :: !DB       -- ^ Block index.
    , _blockDataDir :: !FilePath -- ^ Block and undo files.
    , _epochDataDir :: !FilePath -- ^ Epoch files.
    , _gStateDB     :: !DB       -- ^ Global state corresponding to some tip.
    , _lrcDB        :: !DB       -- ^ Data computed by LRC.
    , _miscDB       :: !DB       -- ^ Everything small and insignificant
    , _epochLock    :: !RWLock   -- ^ Lock for the epoch file consolidation.
    }

makeLenses ''NodeDBs

dbTagToLens :: DBTag -> Lens' NodeDBs DB
dbTagToLens BlockIndexDB = blockIndexDB
dbTagToLens GStateDB     = gStateDB
dbTagToLens LrcDB        = lrcDB
dbTagToLens MiscDB       = miscDB

getNodeDBs :: MonadRealDB ctx m => m NodeDBs
getNodeDBs = view (lensOf @NodeDBs)

getDBByTag :: MonadRealDB ctx m => DBTag -> m DB
getDBByTag tag = view (dbTagToLens tag) <$> getNodeDBs

getBlockIndexDB :: MonadRealDB ctx m => m DB
getBlockIndexDB = getDBByTag BlockIndexDB

getGStateDB :: MonadRealDB ctx m => m DB
getGStateDB = getDBByTag GStateDB

getLrcDB :: MonadRealDB ctx m => m DB
getLrcDB = getDBByTag LrcDB

getMiscDB :: MonadRealDB ctx m => m DB
getMiscDB = getDBByTag MiscDB

class RocksBatchOp a where
    toBatchOp :: a -> [Rocks.BatchOp]

instance RocksBatchOp Rocks.BatchOp where
    toBatchOp = one

data EmptyBatchOp

instance RocksBatchOp EmptyBatchOp where
    toBatchOp _ = []

instance Buildable EmptyBatchOp where
    build _ = ""

data SomeBatchOp =
    forall a. RocksBatchOp a =>
              SomeBatchOp a

instance Semigroup SomeBatchOp where
    a <> b = SomeBatchOp [a, b]

instance Monoid SomeBatchOp where
    mempty = SomeBatchOp ([]::[EmptyBatchOp])
    mappend = (<>)

instance RocksBatchOp SomeBatchOp where
    toBatchOp (SomeBatchOp a) = toBatchOp a

data SomePrettyBatchOp =
    forall a. (RocksBatchOp a, Buildable a) =>
              SomePrettyBatchOp a

instance Semigroup SomePrettyBatchOp where
    a <> b = SomePrettyBatchOp [a, b]

instance Monoid SomePrettyBatchOp where
    mempty = SomePrettyBatchOp ([]::[SomePrettyBatchOp])
    mappend = (<>)

instance RocksBatchOp SomePrettyBatchOp where
    toBatchOp (SomePrettyBatchOp a) = toBatchOp a

instance Buildable SomePrettyBatchOp where
    build (SomePrettyBatchOp x) = Formatting.Buildable.build x

-- instance (Foldable t, RocksBatchOp a) => RocksBatchOp (t a) where
--     toBatchOp = concatMap toBatchOp -- overlapping instances, wtf ?????

instance RocksBatchOp a => RocksBatchOp [a] where
    toBatchOp = concatMap toBatchOp

instance RocksBatchOp a => RocksBatchOp (NonEmpty a) where
    toBatchOp = concatMap toBatchOp

instance Buildable [SomePrettyBatchOp] where
    build = bprint listJson

-- | Write a batch of some operations using 'MonadDB' interface.
-- The only difference from 'dbWriteBatch' is that this function
-- works with whatever types which are instances of 'RocksBatchOp'.
dbWriteBatch' :: (RocksBatchOp a, MonadDB m) => DBTag -> [a] -> m ()
dbWriteBatch' tag batch = dbWriteBatch tag (concatMap toBatchOp batch)

-- | Write a batch of some operations to RocksDB.
rocksWriteBatch :: (RocksBatchOp a, MonadIO m) => [a] -> DB -> m ()
rocksWriteBatch batch DB {..} =
    Rocks.write rocksDB rocksWriteOpts (concatMap toBatchOp batch)

----------------------------------------------------------------------------
-- Opening/options
----------------------------------------------------------------------------

openRocksDB :: MonadIO m => FilePath -> m DB
openRocksDB fp = DB Rocks.defaultReadOptions Rocks.defaultWriteOptions options
                   <$> Rocks.open options
  where options = (Rocks.defaultOptions fp)
          { Rocks.optionsCreateIfMissing = True
          , Rocks.optionsCompression     = Rocks.NoCompression
          }

closeRocksDB :: MonadIO m => DB -> m ()
closeRocksDB = Rocks.close . rocksDB

-- | Open all DBs stored on disk.
-- Don't forget to use 'closeNodeDBs' eventually.
openNodeDBs
    :: (MonadIO m)
    => Bool -> FilePath -> m NodeDBs
openNodeDBs recreate fp = do
    liftIO $
        whenM ((recreate &&) <$> doesDirectoryExist fp) $
        removeDirectoryRecursive fp
    let blocksDir = fp </> "blocks"
    let blocksIndexPath = blocksDir </> "index"
    let _blockDataDir = blocksDir </> "data"
    let _epochDataDir = fp </> "epochs"
    let gStatePath = fp </> "gState"
    let lrcPath = fp </> "lrc"
    let miscPath = fp </> "misc"
    mapM_ ensureDirectoryExists
        [ blocksDir
        , _blockDataDir
        , blocksIndexPath
        , _epochDataDir
        , gStatePath
        , lrcPath
        , miscPath
        ]
    _blockIndexDB <- openRocksDB blocksIndexPath
    _gStateDB <- openRocksDB gStatePath
    _lrcDB <- openRocksDB lrcPath
    _miscDB <- openRocksDB miscPath
    _epochLock <- RWL.new
    pure NodeDBs {..}
  where
    ensureDirectoryExists :: MonadIO m => FilePath -> m ()
    ensureDirectoryExists = liftIO . createDirectoryIfMissing True

-- | Safely close all databases from 'NodeDBs'.
closeNodeDBs :: MonadIO m => NodeDBs -> m ()
closeNodeDBs NodeDBs {..} =
    mapM_ closeRocksDB [_blockIndexDB, _gStateDB, _lrcDB, _miscDB]

deleteNodeDBs :: MonadIO m => NodeDBs -> m ()
deleteNodeDBs =
    liftIO . removeDirectoryRecursive . takeDirectory . _epochDataDir

usingReadOptions
    :: MonadRealDB ctx m
    => Rocks.ReadOptions
    -> ASetter' NodeDBs DB
    -> m a
    -> m a
usingReadOptions opts l =
    local (over (lensOf @NodeDBs . l) (\db -> db {rocksReadOpts = opts}))

usingWriteOptions
    :: MonadRealDB ctx m
    => Rocks.WriteOptions
    -> ASetter' NodeDBs DB
    -> m a
    -> m a
usingWriteOptions opts l =
    local (over (lensOf @NodeDBs . l) (\db -> db {rocksWriteOpts = opts}))

----------------------------------------------------------------------------
-- Reading/writing
----------------------------------------------------------------------------

-- | Read ByteString from RocksDb using given key.
rocksGetBytes :: (MonadIO m) => ByteString -> DB -> m (Maybe ByteString)
rocksGetBytes key DB {..} = Rocks.get rocksDB rocksReadOpts key

-- | Write ByteString to RocksDB for given key.
rocksPutBytes :: (MonadIO m) => ByteString -> ByteString -> DB -> m ()
rocksPutBytes k v DB {..} = Rocks.put rocksDB rocksWriteOpts k v

-- | Delete element from RocksDB for given key.
rocksDelete :: (MonadIO m) => ByteString -> DB -> m ()
rocksDelete k DB {..} = Rocks.delete rocksDB rocksWriteOpts k

-- garbage, should be abstracted and hidden

-- | Write serializable value to RocksDb for given key.
rocksPutBi :: (Bi v, MonadIO m) => ByteString -> v -> DB -> m ()
rocksPutBi k v = rocksPutBytes k (serialize' v)

----------------------------------------------------------------------------
-- Snapshot
----------------------------------------------------------------------------

-- The following is not used in the project yet. So the
-- rocksdb-haskell-ng binding doesn't include it yet, and we simply
-- comment it out here, to be added back at a later stage when needed.

{-
newtype Snapshot = Snapshot Rocks.Snapshot

usingSnapshot
    :: (MonadIO m, MonadMask m)
    => DB -> (Snapshot -> m a) -> m a
usingSnapshot DB {..} action =
    bracket
        (Rocks.createSnapshot rocksDB)
        (Rocks.releaseSnapshot rocksDB)
        (action . Snapshot)
-}

----------------------------------------------------------------------------
-- Iteration
----------------------------------------------------------------------------

-- | Conduit source built from rocks iterator.
rocksIterSource ::
       forall ctx m i.
       ( MonadResource m
       , MonadRealDB ctx m
       , DBIteratorClass i
       , Bi (IterKey i)
       , Bi (IterValue i)
       )
    => DBTag
    -> Proxy i
    -> ConduitT () (IterType i) m ()
rocksIterSource tag _ = do
    DB{..} <- lift $ getDBByTag tag
    let createIter = Rocks.createIter rocksDB rocksReadOpts
    let releaseIter i = Rocks.releaseIter i
    let action iter = do
            Rocks.iterSeek iter (iterKeyPrefix @i)
            produce iter
    bracketP createIter releaseIter action
  where
    produce :: Rocks.Iterator -> ConduitT () (IterType i) m ()
    produce it = do
        entryStr <- processRes =<< Rocks.iterEntry it
        case entryStr of
            Nothing -> pass
            Just e -> do
                yield e
                Rocks.iterNext it
                produce it
    processRes ::
           Maybe (ByteString, ByteString)
        -> ConduitT () (IterType i) m (Maybe (IterType i))
    processRes Nothing   = pure Nothing
    processRes (Just kv) = processIterEntry @i kv

----------------------------------------------------------------------------
-- Implementation/default methods
----------------------------------------------------------------------------

dbGetDefault :: MonadRealDB ctx m => DBTag -> ByteString -> m (Maybe ByteString)
dbGetDefault tag key = getDBByTag tag >>= rocksGetBytes key

dbPutDefault :: MonadRealDB ctx m => DBTag -> ByteString -> ByteString -> m ()
dbPutDefault tag key val = getDBByTag tag >>= rocksPutBytes key val

dbWriteBatchDefault :: MonadRealDB ctx m => DBTag -> [Rocks.BatchOp] -> m ()
dbWriteBatchDefault tag batch = getDBByTag tag >>= rocksWriteBatch batch

dbDeleteDefault :: MonadRealDB ctx m => DBTag -> ByteString -> m ()
dbDeleteDefault tag key = getDBByTag tag >>= rocksDelete key

dbIterSourceDefault ::
       forall ctx m i.
       ( MonadRealDB ctx m
       , MonadResource m
       , DBIteratorClass i
       , Bi (IterKey i)
       , Bi (IterValue i)
       )
    => DBTag
    -> Proxy i
    -> ConduitT () (IterType i) m ()
dbIterSourceDefault = rocksIterSource

----------------------------------------------------------------------------
-- From Pos.DB.Class
----------------------------------------------------------------------------

newtype Serialized a = Serialized
    { unSerialized :: ByteString
    }

data SerBlock
data SerUndo
data SerBlund

type SerializedBlock = Serialized SerBlock
type SerializedUndo = Serialized SerUndo
type SerializedBlund = Serialized SerBlund

-- | Pure read-only interface to the database.
class MonadThrow m => MonadDBRead m where
    -- | This function takes tag and key and reads value associated
    -- with given key from DB corresponding to given tag.
    dbGet :: DBTag -> ByteString -> m (Maybe ByteString)

    -- | Source producing iteration over given 'i'.
    dbIterSource ::
        ( DBIteratorClass i
        , Bi (IterKey i)
        , Bi (IterValue i)
        ) => DBTag -> Proxy i -> ConduitT () (IterType i) (ResourceT m) ()

    -- | Get block by header hash
    dbGetSerBlock :: GenesisHash -> HeaderHash -> m (Maybe SerializedBlock)

    -- | Get undo by header hash
    dbGetSerUndo :: GenesisHash -> HeaderHash -> m (Maybe SerializedUndo)

    -- | Get blund by header hash
    dbGetSerBlund :: GenesisHash -> HeaderHash -> m (Maybe SerializedBlund)

instance {-# OVERLAPPABLE #-}
    (MonadDBRead m, MonadTrans t, MonadThrow (t m)) =>
        MonadDBRead (t m)
  where
    dbGet tag = lift . dbGet tag
    dbIterSource tag (p :: Proxy i) =
        transPipe (transResourceT lift) (dbIterSource tag p)
    dbGetSerBlock genesisHash = lift . dbGetSerBlock genesisHash
    dbGetSerUndo genesisHash = lift . dbGetSerUndo genesisHash
    dbGetSerBlund genesisHash = lift . dbGetSerBlund genesisHash

instance
    (HasLens' r NodeDBs, MonadCatch m, MonadIO m)
    => MonadDBRead (ReaderT r m)
  where
    dbGet = dbGetDefault
    dbIterSource = dbIterSourceDefault
    dbGetSerBlock = dbGetSerBlockRealDefault
    dbGetSerUndo = dbGetSerUndoRealDefault
    dbGetSerBlund = dbGetSerBlundRealDefault

type MonadBlockDBRead m = (MonadDBRead m)

getDeserialized
    :: (MonadBlockDBRead m, Bi v)
    => (x -> m (Maybe (Serialized tag))) -> x -> m (Maybe v)
getDeserialized getter x = getter x >>= \case
    Nothing  -> pure Nothing
    Just ser -> eitherToThrow $ bimap DBMalformed Just $ decodeFull' $ unSerialized ser

getBlock :: MonadBlockDBRead m => GenesisHash -> HeaderHash -> m (Maybe Block)
getBlock = getDeserialized . dbGetSerBlock

-- | Pure interface to the database. Combines read-only interface and
-- ability to put raw bytes.
class MonadDBRead m => MonadDB m where
    -- | This function takes tag, key and value and puts given value
    -- associated with given key into DB corresponding to given tag.
    dbPut :: DBTag -> ByteString -> ByteString -> m ()

    -- | Write batch of operations into DB corresponding to given
    -- tag. This write is atomic.
    --
    -- There are multiple candidates for a type representing batch
    -- operation, for instance:
    -- • Rocks.BatchOp is a good candidate because it's exactly what we want
    -- for pure implementation and it can be used as is for RocksDB.
    -- • We could define our own type for this class, but it would be an
    -- overkill.
    -- • 'SomeBatchOp' could also be used, but it seems to be overcomplication
    -- for such simple interface.
    --
    -- So a list of 'Rocks.BatchOp' was chosen.
    dbWriteBatch :: DBTag -> [Rocks.BatchOp] -> m ()

    -- | This function takes tag and key and deletes value associated
    -- with given key from DB corresponding to given tag.
    dbDelete :: DBTag -> ByteString -> m ()

    -- | Put given blunds into the Block DB.
    dbPutSerBlunds :: NonEmpty (BlockHeader, SerializedBlund) -> m ()

instance {-# OVERLAPPABLE #-}
    (MonadDB m, MonadTrans t, MonadThrow (t m)) =>
        MonadDB (t m)
  where
    dbPut = lift ... dbPut
    dbWriteBatch = lift ... dbWriteBatch
    dbDelete = lift ... dbDelete
    dbPutSerBlunds = lift ... dbPutSerBlunds

----------------------------------------------------------------------------
-- GState abstraction
----------------------------------------------------------------------------

-- | This type class provides functions to get core data from GState.
-- The idea is that actual getters may be defined at high levels, but
-- may be needed at lower levels.
--
-- This class doesn't have a 'MonadRealDB' constraint, because
-- alternative DBs my be used to provide this data.
class Monad m => MonadGState m where
    gsAdoptedBVData :: m BlockVersionData

instance MonadGState ((->) BlockVersionData) where
    gsAdoptedBVData = identity

instance {-# OVERLAPPABLE #-}
    (MonadGState m, MonadTrans t,
     Monad (t m)) =>
        MonadGState (t m)
  where
    gsAdoptedBVData = lift gsAdoptedBVData

gsMaxBlockSize :: MonadGState m => m Byte
gsMaxBlockSize = bvdMaxBlockSize <$> gsAdoptedBVData

gsMaxHeaderSize :: MonadGState m => m Byte
gsMaxHeaderSize = bvdMaxHeaderSize <$> gsAdoptedBVData

gsMaxTxSize :: MonadGState m => m Byte
gsMaxTxSize = bvdMaxTxSize <$> gsAdoptedBVData

gsMaxProposalSize :: MonadGState m => m Byte
gsMaxProposalSize = bvdMaxProposalSize <$> gsAdoptedBVData

gsUnlockStakeEpoch :: MonadGState m => m EpochIndex
gsUnlockStakeEpoch = bvdUnlockStakeEpoch <$> gsAdoptedBVData

-- | Checks if provided epoch is in the bootstrap era.
gsIsBootstrapEra :: MonadGState m => EpochIndex -> m Bool
gsIsBootstrapEra epoch = do
    unlockStakeEpoch <- gsUnlockStakeEpoch
    pure $ isBootstrapEra unlockStakeEpoch epoch

-- from Pos.DB.Functions

-- | Read serialized value (with version) associated with given key from pure DB.
dbGetBi
    :: forall v m.
       (Bi v, MonadDBRead m)
    => DBTag -> ByteString -> m (Maybe v)
dbGetBi tag key =
    dbGet tag key >>= traverse (either throwM pure . dbDecodeIgnoreVersion)

-- | Write serializable value to DB for given key. Uses simple versioning.
dbPutBi :: (Bi v, MonadDB m) => DBTag -> ByteString -> v -> m ()
dbPutBi tag k v = dbPut tag k (serialize' v)

dbDecodeIgnoreVersion :: forall v . Bi v => ByteString -> Either DBError v
dbDecodeIgnoreVersion bytes = case decodeFull' @v bytes of
    Right val -> Right val
    Left _    -> bimap DBMalformed snd $ decodeFull' @(Word8, v) bytes

dbDecodeMaybe :: (Bi v) => ByteString -> Maybe v
dbDecodeMaybe = rightToMaybe . decodeFull'

-- Parse maybe
dbDecodeMaybeWP
    :: forall i . (DBIteratorClass i, Bi (IterKey i))
    => ByteString -> Maybe (IterKey i)
dbDecodeMaybeWP s
    | BS.isPrefixOf (iterKeyPrefix @i) s =
        dbDecodeMaybe . BS.drop (length $ iterKeyPrefix @i) $ s
    | otherwise = Nothing

-- | Encode iterator key using iterator prefix defined in
-- 'DBIteratorClass'.
encodeWithKeyPrefix
    :: forall i . (DBIteratorClass i, Bi (IterKey i))
    => IterKey i -> ByteString
encodeWithKeyPrefix = (iterKeyPrefix @i <>) . serialize'

-- | Given a @(k,v)@ as pair of strings, try to decode both.
processIterEntry ::
       forall i m.
       (Bi (IterKey i), Bi (IterValue i), MonadThrow m, DBIteratorClass i)
    => (ByteString, ByteString)
    -> m (Maybe (IterType i))
processIterEntry (key,val)
    | BS.isPrefixOf prefix key = do
        k <- maybeThrow (DBMalformed $ fmt key "key invalid")
                        (dbDecodeMaybeWP @i key)
        v <- either throwM pure (dbDecodeIgnoreVersion val)
        pure $ Just (k, v)
    | otherwise = pure Nothing
  where
    prefix = iterKeyPrefix @i
    fmt k err =
      sformat
          ("Iterator entry with keyPrefix = "%shown%" is malformed: \
           \key = "%shown%", err: " %string)
           prefix k err
