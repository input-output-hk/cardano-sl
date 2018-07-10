-- | Operations with block index db.

module Pos.DB.BlockIndex
       ( getHeader
       , getTipHeader
       , putHeadersIndex
       , deleteHeaderIndex
       ) where

import           Universum

import           Data.ByteArray (convert)

import qualified Database.RocksDB as Rocks
import           Pos.Core (BlockHeader, HeaderHash, headerHash)
import           Pos.Core.Configuration (CoreConfiguration)
import           Pos.DB.Class (DBTag (BlockIndexDB), MonadBlockDBRead,
                     MonadDB (..))
import           Pos.DB.Functions (dbGetBi, dbSerializeValue)
import           Pos.DB.GState.Common (getTipSomething)

-- | Returns header of block that was requested from Block DB.
getHeader
    :: (MonadBlockDBRead m)
    => CoreConfiguration -> HeaderHash -> m (Maybe BlockHeader)
getHeader cc = dbGetBi cc BlockIndexDB . blockIndexKey

-- | Get 'BlockHeader' corresponding to tip.
getTipHeader :: MonadBlockDBRead m => CoreConfiguration -> m BlockHeader
getTipHeader cc = getTipSomething cc "header" (getHeader cc)

-- | Writes batch of headers into the block index db.
putHeadersIndex :: (MonadDB m) => CoreConfiguration -> [BlockHeader] -> m ()
putHeadersIndex cc =
    dbWriteBatch BlockIndexDB .
    map (\h -> Rocks.Put (blockIndexKey $ headerHash h) (dbSerializeValue cc h))

-- | Deletes header from the index db.
deleteHeaderIndex :: MonadDB m => HeaderHash -> m ()
deleteHeaderIndex = dbDelete BlockIndexDB . blockIndexKey

----------------------------------------------------------------------------
-- Keys
----------------------------------------------------------------------------

blockIndexKey :: HeaderHash -> ByteString
blockIndexKey h = "b" <> convert h
