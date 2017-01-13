{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Extra information for blocks.
--   * Forward links
--   * InMainChain flags

module Pos.DB.GState.BlockExtra
       ( resolveForwardLink
       , isBlockInMainChain
       , BlockExtraOp (..)
       , loadHeadersUpWhile
       , loadBlocksUpWhile
       , prepareGStateBlockExtra
       ) where

import           Control.Lens        (view, (^.))
import           Data.Maybe          (fromJust)
import qualified Database.RocksDB    as Rocks
import           Universum

import           Pos.Binary.Class    (encodeStrict)
import           Pos.DB.Block        (getBlockWithUndo)
import           Pos.DB.Class        (MonadDB, getUtxoDB)
import           Pos.DB.Functions    (RocksBatchOp (..), rocksGetBi, rocksPutBi)
import           Pos.Ssc.Class.Types (Ssc)
import           Pos.Types           (Block, BlockHeader, HasHeaderHash, HeaderHash,
                                      Undo (..), blockHeader, headerHash, prevBlockL)


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
-- Loops on forward links
----------------------------------------------------------------------------

-- Loads something from old to new.
loadUpWhile
    :: forall a b ssc m . (Ssc ssc, MonadDB ssc m, HasHeaderHash a ssc)
    => ((Block ssc, Undo) -> b) -> a -> (b -> Int -> Bool) -> m [b]
loadUpWhile morph start  condition = loadUpWhileDo (headerHash start) 0
  where
    loadUpWhileDo :: HeaderHash ssc -> Int -> m [b]
    loadUpWhileDo curH height = getBlockWithUndo curH >>= \case
        Nothing -> pure []
        Just x@(block,_) -> do
            nextLink <- fmap headerHash <$> resolveForwardLink block
            let curB = morph x
            if | condition curB height && isJust nextLink ->
                 (curB :) <$> loadUpWhileDo (fromJust nextLink) (succ height)
               | condition curB height -> pure [curB]
               | otherwise -> pure []

-- | Returns headers loaded up oldest first.
loadHeadersUpWhile
    :: (Ssc ssc, MonadDB ssc m, HasHeaderHash a ssc)
    => a -> (BlockHeader ssc -> Int -> Bool) -> m [(BlockHeader ssc)]
loadHeadersUpWhile start condition =
    loadUpWhile (view blockHeader . fst) start condition

-- | Returns blocks loaded up oldest first.
loadBlocksUpWhile
    :: (Ssc ssc, MonadDB ssc m, HasHeaderHash a ssc)
    => a -> (Block ssc -> Int -> Bool) -> m [(Block ssc)]
loadBlocksUpWhile start condition = loadUpWhile fst start condition

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
