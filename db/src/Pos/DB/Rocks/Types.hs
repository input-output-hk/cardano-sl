{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Types related to rocksdb implementation 'MonadDBRead' and
-- 'MonadDB'.
--
-- 'MonadRealDB' is the most featured class (actually just a set of
-- constraints) which wraps 'NodeDBs' containing RocksDB
-- databases. This class can be used to manipulate RocksDB
-- directly. It may be useful when you need an access to advanced
-- features of RocksDB.

module Pos.DB.Rocks.Types
       (
         MonadRealDB

       , DB (..)
       , NodeDBs (..)
       , blockIndexDB
       , blockDataDir
       , gStateDB
       , lrcDB
       , miscDB
       , miscLock

       , getDBByTag
       , getNodeDBs
       , getBlockIndexDB
       , getGStateDB
       , getLrcDB
       , getMiscDB
       ) where

import           Universum

import           Control.Lens (makeLenses)
import qualified Database.RocksDB as Rocks

import           Pos.DB.Class (DBTag (..))
import           Pos.Util.Concurrent.RWLock (RWLock)
import           Pos.Util.Util (HasLens (..))


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
    , _gStateDB     :: !DB       -- ^ Global state corresponding to some tip.
    , _lrcDB        :: !DB       -- ^ Data computed by LRC.
    , _miscDB       :: !DB       -- ^ Everything small and insignificant
    , _miscLock     :: !RWLock   -- ^ Lock on misc db
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
