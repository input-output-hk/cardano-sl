{-# LANGUAGE ScopedTypeVariables #-}

-- | Extra information for blocks.
--   * Forward links
--   * InMainChain flags

module Pos.DB.GState.BlockExtra
       ( resolveForwardLink
       , isBlockInMainChain
       , BlockExtraOp (..)
       , foldlUpWhileM
       , loadHeadersUpWhile
       , loadBlocksUpWhile
       , prepareGStateBlockExtra
       ) where

import qualified Data.Text.Buildable
import qualified Database.RocksDB      as Rocks
import           Formatting            (bprint, build, (%))
import           Universum

import           Pos.Binary.Class      (encodeStrict)
import           Pos.Block.Core        (Block, BlockHeader, blockHeader)
import           Pos.Block.Types       (Blund)
import           Pos.Constants         (genesisHash)
import           Pos.Core              (HasHeaderHash, HeaderHash, headerHash)
import           Pos.Crypto            (shortHashF)
import           Pos.DB.Block          (getBlockWithUndo)
import           Pos.DB.Class          (MonadDB, getGStateDB)
import           Pos.DB.Functions      (RocksBatchOp (..), rocksGetBi, rocksPutBi)
import           Pos.Ssc.Class.Helpers (SscHelpersClass)
import           Pos.Util.Chrono       (OldestFirst (..))

----------------------------------------------------------------------------
-- Getters
----------------------------------------------------------------------------

-- | Tries to retrieve next block using current one (given a block/header).
resolveForwardLink
    :: (HasHeaderHash a, MonadDB m)
    => a -> m (Maybe HeaderHash)
resolveForwardLink x =
    rocksGetBi (forwardLinkKey $ headerHash x) =<< getGStateDB

-- | Check if given hash representing block is in main chain.
isBlockInMainChain
    :: (HasHeaderHash a, MonadDB m)
    => a -> m Bool
isBlockInMainChain h = do
    db <- getGStateDB
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

foldlUpWhileM
    :: forall a b ssc m r .
    ( SscHelpersClass ssc
    , MonadDB m
    , HasHeaderHash a
    )
    => (Blund ssc -> m b)
    -> a
    -> (b -> Int -> Bool)
    -> (r -> b -> m r)
    -> r
    -> m r
foldlUpWhileM morphM start condition accM init =
    loadUpWhileDo (headerHash start) 0 init
  where
    loadUpWhileDo :: HeaderHash -> Int -> r -> m r
    loadUpWhileDo curH height !res = getBlockWithUndo curH >>= \case
        Nothing -> pure res
        Just x@(block,_) -> do
            curB <- morphM x
            mbNextLink <- fmap headerHash <$> resolveForwardLink block
            if | not (condition curB height) -> pure res
               | Just nextLink <- mbNextLink -> do
                     newRes <- accM res curB
                     loadUpWhileDo nextLink (succ height) newRes
               | otherwise -> accM res curB

-- Loads something from old to new.
loadUpWhile
    :: forall a b ssc m . (SscHelpersClass ssc, MonadDB m, HasHeaderHash a)
    => (Blund ssc -> b)
    -> a
    -> (b -> Int -> Bool)
    -> m (OldestFirst [] b)
loadUpWhile morph start condition = OldestFirst . reverse <$>
    foldlUpWhileM
        (pure . morph)
        start
        condition
        (\l e -> pure (e : l))
        []

-- | Returns headers loaded up.
loadHeadersUpWhile
    :: (SscHelpersClass ssc, MonadDB m, HasHeaderHash a)
    => a
    -> (BlockHeader ssc -> Int -> Bool)
    -> m (OldestFirst [] (BlockHeader ssc))
loadHeadersUpWhile start condition =
    loadUpWhile (view blockHeader . fst) start condition

-- | Returns blocks loaded up.
loadBlocksUpWhile
    :: (SscHelpersClass ssc, MonadDB m, HasHeaderHash a)
    => a
    -> (Block ssc -> Int -> Bool)
    -> m (OldestFirst [] (Block ssc))
loadBlocksUpWhile start condition = loadUpWhile fst start condition

----------------------------------------------------------------------------
-- Initialization
----------------------------------------------------------------------------

prepareGStateBlockExtra :: MonadDB m => HeaderHash -> m ()
prepareGStateBlockExtra firstGenesisHash = do
    db <- getGStateDB
    rocksPutBi (mainChainKey firstGenesisHash) () db
    rocksPutBi (forwardLinkKey genesisHash) firstGenesisHash db

----------------------------------------------------------------------------
-- Keys
----------------------------------------------------------------------------

forwardLinkKey :: HeaderHash -> ByteString
forwardLinkKey h = "e/fl/" <> encodeStrict h

mainChainKey :: HeaderHash -> ByteString
mainChainKey h = "e/mc/" <> encodeStrict h
