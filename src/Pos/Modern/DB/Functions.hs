-- | Basically wrappers over RocksDB library.

module Pos.Modern.DB.Functions
       ( openDB
       , openNodeDBs
       , rocksGet
       , rocksGetRaw
       , rocksPut
       , rocksPutRaw
       ) where

import           Control.Monad.Trans.Resource (MonadResource)
import           Data.Default                 (def)
import           Database.RocksDB             (getBinary, putBinary)
import qualified Database.RocksDB             as Rocks
import           System.Directory             (createDirectoryIfMissing)
import           System.FilePath              ((</>))
import           Universum

import           Pos.Binary.Class             (Bi, encode)
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

ensureDirectoryExists :: MonadIO m => FilePath -> m ()
ensureDirectoryExists = liftIO . createDirectoryIfMissing True

-- | Read from RocksDb with default options
rocksGetRaw :: (Bi v, MonadIO m) => ByteString -> DB ssc -> m (Maybe v)
rocksGetRaw = notImplemented -- getBinary rocksDB rocksReadOpts key

-- | Read from RocksDb with default options
rocksGet :: (Bi k, Bi v, MonadIO m) => k -> DB ssc -> m (Maybe v)
rocksGet = notImplemented -- getBinary rocksDB rocksReadOpts $ BSL.toStrict (encode key)

-- | Read from RocksDb with default options
rocksPutRaw :: (Bi v, MonadIO m) => ByteString -> v -> DB ssc -> m ()
rocksPutRaw = notImplemented -- putBinary rocksDB rocksWriteOpts key val

-- | Write to RocksDb with default options
rocksPut :: (Bi k, Bi v, MonadIO m) => k -> v -> DB ssc -> m ()
rocksPut = notImplemented -- putBinary rocksDB rocksWriteOpts key val
