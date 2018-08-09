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
import           Pos.DB.Class (DBTag (BlockIndexDB), MonadBlockDBRead, MonadDB (..))
import           Pos.DB.Functions (dbGetBi, dbSerializeValue)
import           Pos.DB.GState.Common (getTipSomething)

-- | Returns header of block that was requested from Block DB.
getHeader
    :: (MonadBlockDBRead m)
    => HeaderHash -> m (Maybe BlockHeader)
getHeader = dbGetBi BlockIndexDB . blockIndexKey

-- | Get 'BlockHeader' corresponding to tip.
getTipHeader :: MonadBlockDBRead m => m BlockHeader
getTipHeader = getTipSomething "header" getHeader

-- | Writes batch of headers into the block index db.
putHeadersIndex :: (MonadDB m) => [BlockHeader] -> m ()
putHeadersIndex =
    dbWriteBatch BlockIndexDB .
    map (\h -> Rocks.Put (blockIndexKey $ headerHash h) (dbSerializeValue h))

-- | Deletes header from the index db.
deleteHeaderIndex :: MonadDB m => HeaderHash -> m ()
deleteHeaderIndex = dbDelete BlockIndexDB . blockIndexKey

----------------------------------------------------------------------------
-- Keys
----------------------------------------------------------------------------

blockIndexKey :: HeaderHash -> ByteString
blockIndexKey h = "b" <> convert h
