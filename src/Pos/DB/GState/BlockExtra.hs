{-# LANGUAGE ScopedTypeVariables #-}

-- | Extra information for blocks.
--   * Forward links.
--   * InMainChain flags.
--   * Slots of the last 'blkSecurityParam' (at most) blocks
--     (for chain quality check).

module Pos.DB.GState.BlockExtra
       ( resolveForwardLink
       , isBlockInMainChain
       , getLastSlots
       , BlockExtraOp (..)
       , foldlUpWhileM
       , loadHeadersUpWhile
       , loadBlocksUpWhile
       , initGStateBlockExtra
       ) where

import           Universum

import qualified Data.Text.Buildable
import qualified Database.RocksDB     as Rocks
import           Formatting           (bprint, build, (%))
import           Serokell.Util.Text   (listJson)

import           Pos.Binary.Class     (encode)
import           Pos.Block.Core       (Block, BlockHeader, blockHeader)
import           Pos.Block.Types      (Blund)
import           Pos.Constants        (genesisHash)
import           Pos.Core             (FlatSlotId, HasHeaderHash, HeaderHash, headerHash,
                                       slotIdF, unflattenSlotId)
import           Pos.Crypto           (shortHashF)
import           Pos.DB               (MonadDB, MonadDBRead, RocksBatchOp (..))
import           Pos.DB.Block         (MonadBlockDB, blkGetBlund)
import           Pos.DB.GState.Common (gsGetBi, gsPutBi)
import           Pos.Util.Chrono      (OldestFirst (..))

----------------------------------------------------------------------------
-- Getters
----------------------------------------------------------------------------

-- | Tries to retrieve next block using current one (given a block/header).
resolveForwardLink
    :: (HasHeaderHash a, MonadDBRead m)
    => a -> m (Maybe HeaderHash)
resolveForwardLink x = gsGetBi (forwardLinkKey $ headerHash x)

-- | Check if given hash representing block is in main chain.
isBlockInMainChain
    :: (HasHeaderHash a, MonadDBRead m)
    => a -> m Bool
isBlockInMainChain h =
    maybe False (\() -> True) <$> gsGetBi (mainChainKey $ headerHash h)

-- | This function returns 'FlatSlotId's of the blocks whose depth
-- less than 'blkSecurityParam'. 'FlatSlotId' is chosen in favor of
-- 'SlotId', because the main use case is chain quality calculation,
-- for which flat slot is more convenient.
getLastSlots :: forall m . MonadDBRead m => m (OldestFirst [] FlatSlotId)
getLastSlots = fromMaybe (OldestFirst []) <$> gsGetBi lastSlotsKey

----------------------------------------------------------------------------
-- BlockOp
----------------------------------------------------------------------------

data BlockExtraOp
    = AddForwardLink HeaderHash
                     HeaderHash
      -- ^ Adds or overwrites forward link
    | RemoveForwardLink HeaderHash
      -- ^ Removes forward link
    | SetInMainChain Bool
                     HeaderHash
      -- ^ Enables or disables "in main chain" status of the block
    | SetLastSlots (OldestFirst [] FlatSlotId)
      -- ^ Updates list of slots for last blocks.
    deriving (Show)

instance Buildable BlockExtraOp where
    build (AddForwardLink from to) =
        bprint ("AddForwardLink from "%shortHashF%" to "%shortHashF) from to
    build (RemoveForwardLink from) =
        bprint ("RemoveForwardLink from "%shortHashF) from
    build (SetInMainChain flag h) =
        bprint ("SetInMainChain for "%shortHashF%": "%build) h flag
    build (SetLastSlots slots) =
        bprint ("SetLastSlots: "%listJson)
        (map (bprint slotIdF . unflattenSlotId) slots)

instance RocksBatchOp BlockExtraOp where
    toBatchOp (AddForwardLink from to) =
        [Rocks.Put (forwardLinkKey from) (encode to)]
    toBatchOp (RemoveForwardLink from) =
        [Rocks.Del $ forwardLinkKey from]
    toBatchOp (SetInMainChain False h) =
        [Rocks.Del $ mainChainKey h]
    toBatchOp (SetInMainChain True h) =
        [Rocks.Put (mainChainKey h) (encode ()) ]
    toBatchOp (SetLastSlots slots) =
        [Rocks.Put lastSlotsKey (encode slots)]

--     -- getOldestFirst is not necessary here, but it ensures that we can't
--     -- change the type and not notice the change
--     toBatchOp (AppendKnownSlots (toList . getOldestFirst -> pairs)) =
--         toBatchOpKnownSlots newOldest prevToDelete pairs
--       where
--         -- newest 'ChainDifficulty' for which we should store slot after
--         -- this operation
--         newNewest :: ChainDifficulty
--         newNewest = fst (last pairs) -- safe here, because list is not empty
--         newOldest :: ChainDifficulty
--         newOldest
--             | newNewest <= blkSecurityParam = 0
--             | otherwise = newNewest - blkSecurityParam
--         -- we need to delete (newOldest - i) for all i in [1 .. length pairs]
--         -- but only if 'i <= newOldest'
--         prevToDelete :: [ChainDifficulty]
--         prevToDelete = mapMaybe prevToDeleteImpl [1 .. genericLength pairs]
--         prevToDeleteImpl i
--             | i > newOldest = Nothing
--             | otherwise = Just (newOldest - i)
--     -- getNewestFirst is not necessary here, but it ensures that we can't
--     -- change the type and not notice the change
--     toBatchOp (PrependKnownSlots tipCD (toList . getNewestFirst -> pairs)) =
--         toBatchOpKnownSlots newOldest prevToDelete pairs
--       where
--         newOldest :: ChainDifficulty
--         newOldest = fst (last pairs) -- safe here, because list is not empty
--         -- newest 'ChainDifficulty' for which we should store slot after
--         -- this operation
--         newNewest :: ChainDifficulty
--         newNewest
--             | tipCD < blkSecurityParam = tipCD
--             | otherwise = tipCD - genericLength pairs
--         prevToDelete :: [ChainDifficulty]
--         prevToDelete = [newNewest + 1, tipCD]

-- toBatchOpKnownSlots ::
--        ChainDifficulty
--        -- ^ Oldest 'ChainDifficulty' for which we should store slot after
--        -- this operation.
--     -> [ChainDifficulty]
--        -- ^ 'ChainDifficulty's for which we want to delete slots.
--     -> [(ChainDifficulty, SlotId)]
--     -> [Rocks.BatchOp]
-- toBatchOpKnownSlots newOldest prevToDelete pairs =
--     Rocks.Put oldestKnownSlotKey (encode newOldest) :
--     map (Rocks.Del . encode) prevToDelete ++
--     map slotPairToPut pairs
--   where
--     slotPairToPut :: (ChainDifficulty, SlotId) -> Rocks.BatchOp
--     slotPairToPut (cd, slot) = Rocks.Put (encode cd) (encode $ flattenSlotId slot)

----------------------------------------------------------------------------
-- Loops on forward links
----------------------------------------------------------------------------

foldlUpWhileM
    :: forall a b ssc m r .
    ( MonadBlockDB ssc m
    , HasHeaderHash a
    )
    => (Blund ssc -> m b)
    -> a
    -> ((Blund ssc, b) -> Int -> Bool)
    -> (r -> b -> m r)
    -> r
    -> m r
foldlUpWhileM morphM start condition accM init =
    loadUpWhileDo (headerHash start) 0 init
  where
    loadUpWhileDo :: HeaderHash -> Int -> r -> m r
    loadUpWhileDo curH height !res = blkGetBlund curH >>= \case
        Nothing -> pure res
        Just x@(block,_) -> do
            curB <- morphM x
            mbNextLink <- fmap headerHash <$> resolveForwardLink block
            if | not (condition (x, curB) height) -> pure res
               | Just nextLink <- mbNextLink -> do
                     newRes <- accM res curB
                     loadUpWhileDo nextLink (succ height) newRes
               | otherwise -> accM res curB

-- Loads something from old to new.
loadUpWhile
    :: forall a b ssc m . (MonadBlockDB ssc m, HasHeaderHash a)
    => (Blund ssc -> b)
    -> a
    -> (b -> Int -> Bool)
    -> m (OldestFirst [] b)
loadUpWhile morph start condition = OldestFirst . reverse <$>
    foldlUpWhileM
        (pure . morph)
        start
        (\b h -> condition (snd b) h)
        (\l e -> pure (e : l))
        []

-- | Returns headers loaded up.
loadHeadersUpWhile
    :: (MonadBlockDB ssc m, HasHeaderHash a)
    => a
    -> (BlockHeader ssc -> Int -> Bool)
    -> m (OldestFirst [] (BlockHeader ssc))
loadHeadersUpWhile start condition =
    loadUpWhile (view blockHeader . fst) start condition

-- | Returns blocks loaded up.
loadBlocksUpWhile
    :: (MonadBlockDB ssc m, HasHeaderHash a)
    => a
    -> (Block ssc -> Int -> Bool)
    -> m (OldestFirst [] (Block ssc))
loadBlocksUpWhile start condition = loadUpWhile fst start condition

----------------------------------------------------------------------------
-- Initialization
----------------------------------------------------------------------------

initGStateBlockExtra :: MonadDB m => HeaderHash -> m ()
initGStateBlockExtra firstGenesisHash = do
    gsPutBi (mainChainKey firstGenesisHash) ()
    gsPutBi (forwardLinkKey genesisHash) firstGenesisHash

----------------------------------------------------------------------------
-- Keys
----------------------------------------------------------------------------

forwardLinkKey :: HeaderHash -> ByteString
forwardLinkKey h = "e/fl/" <> encode h

mainChainKey :: HeaderHash -> ByteString
mainChainKey h = "e/mc/" <> encode h

lastSlotsKey :: ByteString
lastSlotsKey = "e/ls/"
