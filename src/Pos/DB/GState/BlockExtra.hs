-- | Extra information for blocks.
--   * Forward links
--   * TODO InMainChain flags

module Pos.DB.GState.BlockExtra
       ( resolveForwardLink
       , isBlockInMainChain
       , BlockExtraOp (..)
       , prepareGStateBlockExtra
       ) where

import qualified Database.RocksDB    as Rocks
import           Universum

import           Pos.Binary.Class    (encodeStrict)
import           Pos.DB.Class        (MonadDB, getUtxoDB)
import           Pos.DB.Functions    (RocksBatchOp (..), rocksGetBi, rocksPutBi)
import           Pos.Ssc.Class.Types (Ssc)
import           Pos.Types           (Block, HasHeaderHash, HeaderHash, genesisHash,
                                      headerHash)


----------------------------------------------------------------------------
-- Getters
----------------------------------------------------------------------------

-- | Tries to retrieve next block using current one (given a block/header).
resolveForwardLink
    :: (HasHeaderHash a ssc, Ssc ssc, MonadDB ssc m)
    => a -> m (Maybe (Block ssc))
resolveForwardLink x =
    rocksGetBi (forwardLinkKey $ headerHash x) =<< getUtxoDB

-- | Check if given hash representing block is in main chain.
isBlockInMainChain
    :: (HasHeaderHash a ssc, Ssc ssc, MonadDB ssc m)
    => a -> m Bool
isBlockInMainChain h = do
    db <- getUtxoDB
    maybe False (\() -> True) <$> rocksGetBi (mainChainKey $ headerHash h) db

----------------------------------------------------------------------------
-- BlockOp
----------------------------------------------------------------------------

data BlockExtraOp ssc
    = AddForwardLink (HeaderHash ssc) (HeaderHash ssc)
      -- ^ Adds or overwrites forward link
    | RemoveForwardLink (HeaderHash ssc)
      -- ^ Removes forward link
    | SetInMainChain Bool (HeaderHash ssc)
      -- ^ Enables or disables "in main chain" status of the block
    deriving (Show)

instance RocksBatchOp (BlockExtraOp ssc) where
    toBatchOp (AddForwardLink from to) =
        [Rocks.Put (forwardLinkKey from) (encodeStrict to)]
    toBatchOp (RemoveForwardLink from) =
        [Rocks.Del $ forwardLinkKey from]
    toBatchOp (SetInMainChain False h) =
        [Rocks.Del $ mainChainKey h]
    toBatchOp (SetInMainChain True h) =
        [Rocks.Put (mainChainKey h) (encodeStrict ()) ]

----------------------------------------------------------------------------
-- Initialization
----------------------------------------------------------------------------

prepareGStateBlockExtra :: MonadDB ssc m => HeaderHash ssc -> m ()
prepareGStateBlockExtra firstGenesisHash =
    rocksPutBi (mainChainKey firstGenesisHash) () =<< getUtxoDB

----------------------------------------------------------------------------
-- Keys
----------------------------------------------------------------------------

forwardLinkKey :: HeaderHash ssc -> ByteString
forwardLinkKey h = "e/fl" <> encodeStrict h

mainChainKey :: HeaderHash ssc -> ByteString
mainChainKey h = "e/mc" <> encodeStrict h
