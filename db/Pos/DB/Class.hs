{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

-- | A set of type classes which provide access to database.
--
-- 'MonadDBRead' contains reading and iterating capabilities. The
-- advantage of it is that you don't need to do any 'IO' to use it
-- which makes it suitable for pure testing.
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
-- Described two classes have RocksDB implementation "DB.Rocks" and
-- pure one for testing "DB.Pure".
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
       , gsUnlockStakeEpoch
       , gsIsBootstrapEra

         -- * Block DB
       , MonadBlockDBGeneric (..)
       , dbGetBlund
       , MonadBlockDBGenericWrite (..)
       ) where

import           Universum

import           Control.Monad.Morph          (hoist)
import           Control.Monad.Trans          (MonadTrans (..))
import           Control.Monad.Trans.Control  (MonadBaseControl)
import           Control.Monad.Trans.Resource (ResourceT)
import           Data.Conduit                 (Source)
import qualified Database.RocksDB             as Rocks
import           Serokell.Data.Memory.Units   (Byte)

import           Pos.Binary.Class             (Bi)
import           Pos.Core                     (BlockVersionData (..), EpochIndex,
                                               HeaderHash, isBootstrapEra)

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
    return $ isBootstrapEra unlockStakeEpoch epoch

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
    (MonadBlockDBGeneric header blk undo m, MonadTrans t,
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

-- | Superclass of 'MonadBlockDB' which allows to modify the Block
-- DB.
--
-- TODO: support deletion when we actually start using deletion
-- (probably not soon).
class MonadBlockDBGeneric header blk undo m => MonadBlockDBGenericWrite header blk undo m where
    -- | Put given blund into the Block DB.
    dbPutBlund :: (blk,undo) -> m ()

instance {-# OVERLAPPABLE #-}
    ( MonadBlockDBGenericWrite header blk undo m
    , MonadBlockDBGeneric header blk undo (t m)
    , MonadTrans t
    ) =>
        MonadBlockDBGenericWrite header blk undo (t m)
  where
    dbPutBlund = lift . dbPutBlund
