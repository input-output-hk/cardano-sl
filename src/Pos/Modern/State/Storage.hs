{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
module Pos.Modern.State.Storage
       (
         MonadDB (..)
       , DB (..)
       , NodeState (..)
       , blockDb
       , utxoDb
       , openNodeDB
       , rocksGet
       , rocksGetRaw
       , rocksPut
       , rocksPutRaw
       ) where

import           Control.Lens                 (makeLenses)
import           Control.Monad.Trans.Resource (MonadResource)
import           Data.Binary                  (Binary)
import           Data.Default                 (def)
import           Database.RocksDB             (getBinary, putBinary)
import qualified Database.RocksDB             as Rocks
import           Universum

import           Pos.Util                     (binaryToBS)

class MonadIO m => MonadDB ssc m | m -> ssc where
    getBlockDB :: m (DB ssc)
    getUtxoDB  :: m (DB ssc)
    getUndoDB  :: m (DB ssc)
    usingReadOptionsUtxo   :: Rocks.ReadOptions -> m a -> m a
    usingWriteOptionsUtxo  :: Rocks.WriteOptions -> m a -> m a
    usingReadOptionsBlock  :: Rocks.ReadOptions -> m a -> m a
    usingWriteOptionsBlock :: Rocks.WriteOptions -> m a -> m a

data DB ssc = DB
    {
      rocksReadOpts  :: !Rocks.ReadOptions -- should we replace `rocks` prefix by other or remove it at all?
    , rocksWriteOpts :: !Rocks.WriteOptions
    , rocksOptions   :: !Rocks.Options
    , rocksDb        :: !Rocks.DB
    }

openNodeDB :: MonadResource m => FilePath -> m (DB ssc)
openNodeDB fp = DB def def def
                   <$> Rocks.open fp def { Rocks.createIfMissing = True }

data NodeState ssc = NodeState
    {
      _blockDb :: DB ssc -- blocks stuff: blocks, block index, undo
    , _utxoDb  :: DB ssc -- tx stuff
    }
makeLenses ''NodeState

-- | Read from RocksDb with default options
rocksGetRaw :: (Binary v, MonadIO m) => ByteString -> DB ssc -> m (Maybe v)
rocksGetRaw key DB {..} = getBinary rocksDb rocksReadOpts key

-- | Read from RocksDb with default options
rocksGet :: (Binary k, Binary v, MonadIO m) => k -> DB ssc -> m (Maybe v)
rocksGet key DB {..} = getBinary rocksDb rocksReadOpts (binaryToBS key)

-- | Read from RocksDb with default options
rocksPutRaw :: (Binary v, MonadIO m) => ByteString -> v -> DB ssc -> m ()
rocksPutRaw key val DB {..} = putBinary rocksDb rocksWriteOpts key val

-- | Write to RocksDb with default options
rocksPut :: (Binary k, Binary v, MonadIO m) => k -> v -> DB ssc -> m ()
rocksPut key val DB {..} = putBinary rocksDb rocksWriteOpts key val
