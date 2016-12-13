{-# LANGUAGE ScopedTypeVariables #-}

-- | Basically wrappers over RocksDB library.

module Pos.Modern.DB.Functions
       ( openDB
       , openNodeDBs
       , rocksGetBi
       , rocksGetBytes
       , rocksPutBi
       , rocksPutBytes
       ) where

import           Control.Monad.Fail           (fail)
import           Control.Monad.TM             ((.>>=.))
import           Control.Monad.Trans.Resource (MonadResource)
import qualified Data.ByteString.Lazy         as BSL
import           Data.Default                 (def)
import qualified Database.RocksDB             as Rocks
import           Formatting                   (formatToString, shown, string, (%))
import           System.Directory             (createDirectoryIfMissing)
import           System.FilePath              ((</>))
import           Universum

import           Pos.Binary.Class             (Bi, decodeFull, encode)
import           Pos.Modern.DB.Types          (DB (..), NodeDBs (..))

-- | Open DB stored on disk.
openDB :: MonadResource m => FilePath -> m (DB ssc)
openDB fp = DB def def def
                   <$> Rocks.open fp def { Rocks.createIfMissing = True }

-- | Open all DBs stored on disk.
openNodeDBs :: MonadResource m => FilePath -> m (NodeDBs ssc)
openNodeDBs fp = do
    let blockPath = fp </> "blocks"
    let utxoPath = fp </> "utxo"
    mapM_ ensureDirectoryExists [blockPath, utxoPath]
    NodeDBs <$> openDB (fp </> "blocks") <*> openDB (fp </> "utxo")
  where
    ensureDirectoryExists :: MonadIO m => FilePath -> m ()
    ensureDirectoryExists = liftIO . createDirectoryIfMissing True

-- | Read ByteString from RocksDb using given key.
rocksGetBytes :: (MonadIO m) => ByteString -> DB ssc -> m (Maybe ByteString)
rocksGetBytes key DB {..} = Rocks.get rocksDB rocksReadOpts key

-- | Read serialized value from RocksDB using given key.
rocksGetBi
    :: forall v m ssc.
       (Bi v, MonadIO m)
    => ByteString -> DB ssc -> m (Maybe v)
rocksGetBi key db = do
    bytes <- rocksGetBytes key db
    bytes .>>=. decodeValue
  where
    onParseError msg =
        liftIO . fail $
        formatToString
            ("rocksGetBi: stored value is malformed, key = " %shown %
             ", err: " %string)
            key
            msg
    decodeValue :: ByteString -> m (Maybe v)
    decodeValue = either onParseError (pure . Just) . decodeFull . BSL.fromStrict

-- | Write ByteString to RocksDB for given key.
rocksPutBytes :: (MonadIO m) => ByteString -> ByteString -> DB ssc -> m ()
rocksPutBytes k v DB {..} = Rocks.put rocksDB rocksWriteOpts k v

-- | Write serializable value to RocksDb for given key.
rocksPutBi :: (Bi v, MonadIO m) => ByteString -> v -> DB ssc -> m ()
rocksPutBi k v = rocksPutBytes k (BSL.toStrict $ encode v)
