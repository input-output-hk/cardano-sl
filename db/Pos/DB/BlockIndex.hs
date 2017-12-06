-- | Operations with block index db.

module Pos.DB.BlockIndex
       ( getHeader
       , getTipHeader
       , blockIndexKey
       ) where

import           Universum

import           Data.ByteArray (convert)

import           Pos.Core.Block (BlockHeader)
import           Pos.Core.Common (HeaderHash)
import           Pos.Core.Configuration (HasConfiguration)
import           Pos.DB.Class (DBTag (BlockIndexDB), MonadBlockDBRead)
import           Pos.DB.Functions (dbGetBi)
import           Pos.DB.GState.Common (getTipSomething)

-- | Returns header of block that was requested from Block DB.
getHeader
    :: (HasConfiguration, MonadBlockDBRead m)
    => HeaderHash -> m (Maybe BlockHeader)
getHeader = dbGetBi BlockIndexDB . blockIndexKey

-- | Get 'BlockHeader' corresponding to tip.
getTipHeader :: MonadBlockDBRead m => m BlockHeader
getTipHeader = getTipSomething "header" getHeader

----------------------------------------------------------------------------
-- Keys
----------------------------------------------------------------------------

blockIndexKey :: HeaderHash -> ByteString
blockIndexKey h = "b" <> convert h
