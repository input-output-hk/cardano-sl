-- | Types related to DB.

module Pos.DB.Types
       (
         -- * General types.
         DB (..)
       , NodeDBs (..)
       , blockIndexDB
       , blockDataDir
       , gStateDB
       , lrcDB
       , miscDB
       , miscLock

       -- * Snapshot
       , Snapshot (..)
       , usingSnapshot
       ) where

import           Control.Lens               (makeLenses)
import qualified Database.RocksDB           as Rocks
import           Pos.Util.Concurrent.RWLock (RWLock)
import           Universum

----------------------------------------------------------------------------
-- General
----------------------------------------------------------------------------

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

----------------------------------------------------------------------------
-- Snapshot
----------------------------------------------------------------------------

newtype Snapshot = Snapshot Rocks.Snapshot

usingSnapshot
    :: (MonadIO m, MonadMask m)
    => DB -> (Snapshot -> m a) -> m a
usingSnapshot DB {..} action =
    bracket
        (Rocks.createSnapshot rocksDB)
        (Rocks.releaseSnapshot rocksDB)
        (action . Snapshot)
