{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

-- | A set of type classes which provide access to database.
--
-- 'MonadRealDB' is the most featured class (actually just a set of
-- constraints) which wraps 'NodeDBs' which contains RocksDB
-- databases. This class can be used to manipulate RocksDB
-- directly. It may be useful when you need an access to advanced
-- features of RocksDB.
--
-- Apart from that we have few more classes here.
--
-- 'MonadDBRead' contains only 'dbGet' method.  The advantage of it is
-- that you don't need to do any 'IO' to use it which makes it
-- suitable for pure testing. TODO: add iteration abilitiy.
--
-- 'MonadDB' is a superclass of 'MonadDB' and allows to modify
-- DB. Again, its purpose is to make it possible to use DB w/o IO
-- context.
--
-- 'MonadGState' contains functions to retrieve some data from
-- GState DB without knowledge of where this data is located (where in
-- code and where in DB, i. e. by which key). For example, if X wants
-- to get data maintained by Y and doesn't know about Y, it can use
-- 'MonadGState' (which is at pretty low level).
--
-- 'MonadBlockDBGeneric' contains functions which provide read-only
-- access to the Block DB.
-- For this DB we don't want to use 'MonadRealDB' for several reasons:
-- • we store blocks and undos in files, not in key-value storage;
-- • there are only three getters, so it's not a big problem to make all of
-- them part of type class;
-- • some parts of code need to access Block DB but they don't know actual
-- type of 'Block' and/or 'BlockHeader' as well as 'Undo', so 'MonadBlockDB'
-- is parameterized by these types.

module Pos.DB.Class
       (
         -- * Pure
         DBTag (..)
       , dbTagToLens
       , DBIteratorClass (..)
       , IterType
       , MonadDBRead (..)
       , MonadDB (..)

         -- * GState
       , MonadGState (..)
       , gsMaxBlockSize
       , gsMaxHeaderSize
       , gsMaxTxSize
       , gsMaxProposalSize

         -- * Block DB
       , MonadBlockDBGeneric (..)
       , dbGetBlund

         -- * RocksDB
       , MonadRealDB
       , getNodeDBs
       , usingReadOptions
       , usingWriteOptions
       , getBlockIndexDB
       , getGStateDB
       , getLrcDB
       , getMiscDB
       ) where

import           Universum

import           Control.Lens                   (ASetter')
import           Control.Monad.Morph            (hoist)
import           Control.Monad.Trans            (MonadTrans (..))
import           Control.Monad.Trans.Control    (MonadBaseControl)
import           Control.Monad.Trans.Lift.Local (LiftLocal (..))
import           Control.Monad.Trans.Resource   (ResourceT)
import           Data.Conduit                   (Source)
import qualified Database.RocksDB               as Rocks
import           Ether.Internal                 (HasLens (..))
import           Serokell.Data.Memory.Units     (Byte)

import           Pos.Binary.Class               (Bi)
import           Pos.Core                       (BlockVersionData (..), HeaderHash)
import           Pos.DB.Types                   (DB (..), NodeDBs, blockIndexDB, gStateDB,
                                                 lrcDB, miscDB)

----------------------------------------------------------------------------
-- Pure
----------------------------------------------------------------------------

-- | Tag which denotes one of DBs used by application.
data DBTag
    = BlockIndexDB
    | GStateDB
    | LrcDB
    | MiscDB
    deriving (Eq)

dbTagToLens :: DBTag -> Lens' NodeDBs DB
dbTagToLens BlockIndexDB = blockIndexDB
dbTagToLens GStateDB     = gStateDB
dbTagToLens LrcDB        = lrcDB
dbTagToLens MiscDB       = miscDB

-- | Key-value type family encapsulating the iterator (something we
-- can iterate on) functionality.
class DBIteratorClass i where
    type IterKey   i :: *
    type IterValue i :: *
    iterKeyPrefix :: ByteString

type IterType i = (IterKey i, IterValue i)

-- | Pure read-only interface to the database.
class (MonadBaseControl IO m, MonadThrow m) => MonadDBRead m where
    -- | This function takes tag and key and reads value associated
    -- with given key from DB corresponding to given tag.
    dbGet :: DBTag -> ByteString -> m (Maybe ByteString)

    -- | Source producing iteration over given 'i'.
    dbIterSource ::
        ( DBIteratorClass i
        , Bi (IterKey i)
        , Bi (IterValue i)
        ) => DBTag -> Proxy i -> Source (ResourceT m) (IterType i)

instance {-# OVERLAPPABLE #-}
    (MonadDBRead m, MonadTrans t, MonadThrow (t m), MonadBaseControl IO (t m)) =>
        MonadDBRead (t m)
  where
    dbGet tag = lift . dbGet tag
    dbIterSource tag (p :: Proxy i) =
        hoist (hoist lift) (dbIterSource tag p)

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


instance {-# OVERLAPPABLE #-}
    (MonadDB m, MonadTrans t, MonadThrow (t m), MonadBaseControl IO (t m)) =>
        MonadDB (t m)
  where
    dbPut = lift ... dbPut
    dbWriteBatch = lift ... dbWriteBatch
    dbDelete = lift ... dbDelete

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

instance {-# OVERLAPPABLE #-}
    (MonadGState m, MonadTrans t, LiftLocal t,
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

----------------------------------------------------------------------------
-- Block DB abstraction
----------------------------------------------------------------------------

-- | Monad which provides read-only access to the Block DB. It's
-- generic in a way that it allows to specify different types of
-- block|header|undo. Read rationale behind this type in the
-- documentation of this module.
class MonadDBRead m =>
      MonadBlockDBGeneric header blk undo m | blk -> header, blk -> undo where
    dbGetHeader :: HeaderHash -> m (Maybe header)
    dbGetBlock :: HeaderHash -> m (Maybe blk)
    dbGetUndo :: HeaderHash -> m (Maybe undo)

instance {-# OVERLAPPABLE #-}
    (MonadBlockDBGeneric header blk undo m, MonadTrans t, LiftLocal t,
     MonadDBRead (t m)) =>
        MonadBlockDBGeneric header blk undo (t m)
  where
    dbGetHeader = lift . dbGetHeader @header @blk @undo
    dbGetBlock = lift . dbGetBlock @header @blk @undo
    dbGetUndo = lift . dbGetUndo @header @blk @undo

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

----------------------------------------------------------------------------
-- RocksDB
----------------------------------------------------------------------------

-- | This is the set of constraints necessary to operate on «real» DBs
-- (which are wrapped into 'NodeDBs').  Apart from providing access to
-- 'NodeDBs' it also has 'MonadIO' constraint, because it's impossible
-- to use real DB without IO. Finally, it has 'MonadCatch' constraints
-- (partially for historical reasons, partially for good ones).
type MonadRealDB ctx m
     = (MonadReader ctx m, HasLens NodeDBs ctx NodeDBs, MonadIO m, MonadBaseControl IO m, MonadCatch m)

getNodeDBs :: MonadRealDB ctx m => m NodeDBs
getNodeDBs = view (lensOf @NodeDBs)

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

getBlockIndexDB :: MonadRealDB ctx m => m DB
getBlockIndexDB = view blockIndexDB <$> getNodeDBs

getGStateDB :: MonadRealDB ctx m => m DB
getGStateDB = view gStateDB <$> getNodeDBs

getLrcDB :: MonadRealDB ctx m => m DB
getLrcDB = view lrcDB <$> getNodeDBs

getMiscDB :: MonadRealDB ctx m => m DB
getMiscDB = view miscDB <$> getNodeDBs
