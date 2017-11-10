{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE Rank2Types          #-}
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
       , MonadBlockDBRead
       , getBlock
       , MonadDB (..)

         -- * GState
       , MonadGState (..)
       , gsMaxBlockSize
       , gsMaxHeaderSize
       , gsMaxTxSize
       , gsMaxProposalSize
       , gsUnlockStakeEpoch
       , gsIsBootstrapEra
       ) where

import           Universum

import           Control.Monad.Morph (hoist)
import           Control.Monad.Trans (MonadTrans (..))
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.Trans.Resource (ResourceT)
import           Data.Conduit (Source)
import qualified Database.RocksDB as Rocks
import           Serokell.Data.Memory.Units (Byte)

import           Pos.Binary.Class (Bi, decodeFull)
import           Pos.Binary.Core.Blockchain ()
import           Pos.Core (Block, BlockVersionData (..), EpochIndex, HasConfiguration, HeaderHash,
                           isBootstrapEra)
import           Pos.Core.Block (BlockchainHelpers, MainBlockchain)

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
class (HasConfiguration, MonadBaseControl IO m, MonadThrow m) => MonadDBRead m where
    -- | This function takes tag and key and reads value associated
    -- with given key from DB corresponding to given tag.
    dbGet :: DBTag -> ByteString -> m (Maybe ByteString)

    -- | Source producing iteration over given 'i'.
    dbIterSource ::
        ( DBIteratorClass i
        , Bi (IterKey i)
        , Bi (IterValue i)
        ) => DBTag -> Proxy i -> Source (ResourceT m) (IterType i)

    -- | Get block by header hash
    dbGetRawBlock :: HeaderHash -> m (Maybe ByteString)

    -- | Get undo by header hash
    dbGetRawUndo :: HeaderHash -> m (Maybe ByteString)

instance {-# OVERLAPPABLE #-}
    (MonadDBRead m, MonadTrans t, MonadThrow (t m), MonadBaseControl IO (t m)) =>
        MonadDBRead (t m)
  where
    dbGet tag = lift . dbGet tag
    dbIterSource tag (p :: Proxy i) =
        hoist (hoist lift) (dbIterSource tag p)
    dbGetRawBlock = lift . dbGetRawBlock
    dbGetRawUndo = lift . dbGetRawUndo


type MonadBlockDBRead m = (MonadDBRead m, BlockchainHelpers MainBlockchain)

getBlock :: MonadBlockDBRead m => HeaderHash -> m (Maybe Block)
getBlock x = do
    mBS <- dbGetRawBlock x
    pure $ rightToMaybe . decodeFull =<< mBS

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

    -- | Put given blund into the Block DB.
    dbPutRawBlund :: (Block, ByteString) -> m ()

instance {-# OVERLAPPABLE #-}
    (MonadDB m, MonadTrans t, MonadThrow (t m), MonadBaseControl IO (t m)) =>
        MonadDB (t m)
  where
    dbPut = lift ... dbPut
    dbWriteBatch = lift ... dbWriteBatch
    dbDelete = lift ... dbDelete
    dbPutRawBlund = lift ... dbPutRawBlund

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
