-- | Operations with block index db.

module Pos.DB.BlockIndex
       ( getHeader
       , getTipHeader
       ) where

import           Universum

import           Data.ByteArray (convert)
import           Formatting (sformat, stext, (%))

import           Pos.Core.Block (BlockHeader, BlockchainHelpers, MainBlockchain)
import           Pos.Core.Configuration (HasConfiguration)
import           Pos.Core.Types (HeaderHash)

import           Pos.DB.Class (DBTag (BlockIndexDB), MonadDBRead (..))
import           Pos.DB.Error (DBError (DBMalformed))
import           Pos.DB.Functions (dbGetBi)
import           Pos.DB.GState.Common (getTip)

-- | Returns header of block that was requested from Block DB.
getHeader
    :: (HasConfiguration, MonadDBRead m, BlockchainHelpers MainBlockchain)
    => HeaderHash -> m (Maybe BlockHeader)
getHeader = dbGetBi BlockIndexDB . blockIndexKey

-- | Get 'BlockHeader' corresponding to tip.
getTipHeader
    :: (MonadDBRead m, BlockchainHelpers MainBlockchain)
    => m BlockHeader
getTipHeader = getTipSomething "header" getHeader

getTipSomething
    :: forall m smth.
       MonadDBRead m
    => Text -> (HeaderHash -> m (Maybe smth)) -> m smth
getTipSomething smthDescription smthGetter =
    maybe onFailure pure =<< smthGetter =<< getTip
  where
    fmt = "there is no "%stext%" corresponding to tip"
    onFailure = throwM $ DBMalformed $ sformat fmt smthDescription

----------------------------------------------------------------------------
-- Keys
----------------------------------------------------------------------------

blockIndexKey :: HeaderHash -> ByteString
blockIndexKey h = "b" <> convert h
