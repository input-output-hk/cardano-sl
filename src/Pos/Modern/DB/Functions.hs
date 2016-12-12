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
import           Data.Binary                  (Binary)
import           Data.Default                 (def)
import           Database.RocksDB             (getBinary, putBinary)
import qualified Database.RocksDB             as Rocks
import           System.Directory             (createDirectoryIfMissing)
import           System.FilePath              ((</>))
import           Universum

import           Pos.Modern.DB.Types          (DB (..), NodeDBs (..))
import           Pos.Util                     (binaryToBS)

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
rocksGetRaw :: (Binary v, MonadIO m) => ByteString -> DB ssc -> m (Maybe v)
rocksGetRaw key DB {..} = getBinary rocksDB rocksReadOpts key

-- | Read from RocksDb with default options
rocksGet :: (Binary k, Binary v, MonadIO m) => k -> DB ssc -> m (Maybe v)
rocksGet key DB {..} = getBinary rocksDB rocksReadOpts (binaryToBS key)

-- | Read from RocksDb with default options
rocksPutRaw :: (Binary v, MonadIO m) => ByteString -> v -> DB ssc -> m ()
rocksPutRaw key val DB {..} = putBinary rocksDB rocksWriteOpts key val

-- | Write to RocksDb with default options
rocksPut :: (Binary k, Binary v, MonadIO m) => k -> v -> DB ssc -> m ()
rocksPut key val DB {..} = putBinary rocksDB rocksWriteOpts key val
