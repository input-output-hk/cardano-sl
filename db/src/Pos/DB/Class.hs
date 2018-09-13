{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

-- | A set of type classes which provide access to database.
--
-- 'MonadDBRead' contains reading and iterating capabilities. The
-- advantage of it is that you don't need to do any 'IO' to use it
-- which makes it suitable for pure testing.
-- 'MonadDBRead' also provides access to the Block DB.
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

module Pos.DB.Class
       (
         -- * Pure
         DBTag (..)
       , DBIteratorClass (..)
       , IterType
       , MonadDBRead (..)
       , Serialized (..)
       , SerializedBlock
       , SerializedUndo
       , SerializedBlund
       , MonadBlockDBRead
       , getDeserialized
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

import           Control.Monad.Trans (MonadTrans (..))
import           Control.Monad.Trans.Resource (ResourceT, transResourceT)
import           Data.Conduit (ConduitT, transPipe)
import qualified Database.RocksDB as Rocks
import           Serokell.Data.Memory.Units (Byte)

import           Pos.Binary.Class (Bi, decodeFull')
import           Pos.Chain.Block (Block, BlockHeader, HeaderHash)
import           Pos.Core (EpochIndex, GenesisHash, isBootstrapEra)
import           Pos.Core.Update (BlockVersionData (..))
import           Pos.DB.Error (DBError (DBMalformed))
import           Pos.Util.Util (eitherToThrow)

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
