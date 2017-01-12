-- | Extra information for blocks.
--   * Forward links
--   * TODO InMainChain flags

module Pos.DB.GState.BlockExtra
       ( resolveForwardLink
       , BlockExtraOp (..)
       ) where

import qualified Database.RocksDB    as Rocks
import           Universum

import           Pos.Binary.Class    (encodeStrict)
import           Pos.DB.Class        (MonadDB, getUtxoDB)
import           Pos.DB.Functions    (RocksBatchOp (..), rocksGetBi)
import           Pos.Ssc.Class.Types (Ssc)
import           Pos.Types           (Block, HasHeaderHash, HeaderHash, headerHash)


----------------------------------------------------------------------------
-- Getters
----------------------------------------------------------------------------

-- | Tries to retrieve next block using current one (given a block/header).
resolveForwardLink
    :: (HasHeaderHash a ssc, Ssc ssc, MonadDB ssc m)
    => a -> m (Maybe (Block ssc))
resolveForwardLink x =
    rocksGetBi (forwardLinkKey $ headerHash x) =<< getUtxoDB

----------------------------------------------------------------------------
-- BlockOp
----------------------------------------------------------------------------

data BlockExtraOp ssc
    = BlockExtraAddForwardLink (HeaderHash ssc) (HeaderHash ssc)
      -- ^ Adds or overwrites forward link
    | BlockExtraRemoveForwardLink (HeaderHash ssc)
      -- ^ Removes forward link
    deriving (Show)

instance RocksBatchOp (BlockExtraOp ssc) where
    toBatchOp (BlockExtraAddForwardLink from to) =
        [Rocks.Put (forwardLinkKey from) (encodeStrict to)]
    toBatchOp (BlockExtraRemoveForwardLink from) =
        [Rocks.Del $ forwardLinkKey from]

----------------------------------------------------------------------------
-- Keys
----------------------------------------------------------------------------

forwardLinkKey :: HeaderHash ssc -> ByteString
forwardLinkKey h = "e/fl" <> encodeStrict h
