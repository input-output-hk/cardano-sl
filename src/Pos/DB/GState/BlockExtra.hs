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

import qualified Data.Text.Buildable
import qualified Database.RocksDB    as Rocks
import           Formatting          (bprint, build, (%))
import           Universum

import           Pos.Binary.Class    (encodeStrict)
import           Pos.Block.Types     (Blund)
import           Pos.Crypto          (shortHashF)
import           Pos.DB.Block        (getBlockWithUndo)
import           Pos.DB.Class        (MonadDB, getUtxoDB)
import           Pos.DB.Functions    (RocksBatchOp (..), rocksGetBi, rocksPutBi)
import           Pos.Ssc.Class.Types (Ssc)
import           Pos.Types           (Block, BlockHeader, HasHeaderHash, HeaderHash,
                                      blockHeader, headerHash)
import           Pos.Util            (OldestFirst (..))


----------------------------------------------------------------------------
-- Getters
----------------------------------------------------------------------------

-- | Tries to retrieve next block using current one (given a block/header).
resolveForwardLink
    :: (HasHeaderHash a, MonadDB ssc m)
    => a -> m (Maybe HeaderHash)
resolveForwardLink x =
    rocksGetBi (forwardLinkKey $ headerHash x) =<< getUtxoDB

-- | Check if given hash representing block is in main chain.
isBlockInMainChain
    :: (HasHeaderHash a, MonadDB ssc m)
    => a -> m Bool
isBlockInMainChain h = do
    db <- getUtxoDB
    maybe False (\() -> True) <$> rocksGetBi (mainChainKey $ headerHash h) db

----------------------------------------------------------------------------
-- BlockOp
----------------------------------------------------------------------------

data BlockExtraOp
    = AddForwardLink HeaderHash HeaderHash
      -- ^ Adds or overwrites forward link
    | RemoveForwardLink HeaderHash
      -- ^ Removes forward link
    | SetInMainChain Bool HeaderHash
      -- ^ Enables or disables "in main chain" status of the block
    deriving (Show)

instance Buildable BlockExtraOp where
    build (AddForwardLink from to) =
        bprint ("AddForwardLink from "%shortHashF%" to "%shortHashF) from to
    build (RemoveForwardLink from) =
        bprint ("RemoveForwardLink from "%shortHashF) from
    build (SetInMainChain flag h) =
        bprint ("SetInMainChain for "%shortHashF%": "%build) h flag

instance RocksBatchOp BlockExtraOp where
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
    :: forall a b ssc m . (Ssc ssc, MonadDB ssc m, HasHeaderHash a)
    => (Blund ssc -> b)
    -> a
    -> (b -> Int -> Bool)
    -> m (OldestFirst [] b)
loadUpWhile morph start condition =
    OldestFirst <$> loadUpWhileDo (headerHash start) 0
  where
    loadUpWhileDo :: HeaderHash -> Int -> m [b]
    loadUpWhileDo curH height = getBlockWithUndo curH >>= \case
        Nothing -> pure []
        Just x@(block,_) -> do
            mbNextLink <- fmap headerHash <$> resolveForwardLink block
            let curB = morph x
            if | not (condition curB height) -> pure []
               | Just nextLink <- mbNextLink ->
                     (curB :) <$> loadUpWhileDo nextLink (succ height)
               | otherwise -> pure [curB]

-- | Returns headers loaded up.
loadHeadersUpWhile
    :: (Ssc ssc, MonadDB ssc m, HasHeaderHash a)
    => a
    -> (BlockHeader ssc -> Int -> Bool)
    -> m (OldestFirst [] (BlockHeader ssc))
loadHeadersUpWhile start condition =
    loadUpWhile (view blockHeader . fst) start condition

-- | Returns blocks loaded up.
loadBlocksUpWhile
    :: (Ssc ssc, MonadDB ssc m, HasHeaderHash a)
    => a
    -> (Block ssc -> Int -> Bool)
    -> m (OldestFirst [] (Block ssc))
loadBlocksUpWhile start condition = loadUpWhile fst start condition

----------------------------------------------------------------------------
-- Initialization
----------------------------------------------------------------------------

prepareGStateBlockExtra :: MonadDB ssc m => HeaderHash -> m ()
prepareGStateBlockExtra firstGenesisHash =
    rocksPutBi (mainChainKey firstGenesisHash) () =<< getUtxoDB

----------------------------------------------------------------------------
-- Keys
----------------------------------------------------------------------------

forwardLinkKey :: HeaderHash -> ByteString
forwardLinkKey h = "e/fl" <> encodeStrict h

mainChainKey :: HeaderHash -> ByteString
mainChainKey h = "e/mc" <> encodeStrict h
