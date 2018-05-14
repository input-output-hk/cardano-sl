-- | Extra information for blocks.
--   * Forward links.
--   * InMainChain flags.
--   * Slots of the last 'blkSecurityParam' (at most) blocks
--     (for chain quality check).

module Pos.GState.BlockExtra
       ( resolveForwardLink
       , isBlockInMainChain
       , getLastSlots
       , getFirstGenesisBlockHash
       , BlockExtraOp (..)
       , foldlUpWhileM
       , loadHashesUpWhile
       , loadHeadersUpWhile
       , loadBlocksUpWhile
       , initGStateBlockExtra
       ) where

import           Universum

import qualified Data.Text.Buildable
import qualified Database.RocksDB as Rocks
import           Formatting (bprint, build, (%))
import           Serokell.Util.Text (listJson)

import           Pos.Binary.Class (serialize')
import           Pos.Block.Slog.Types (LastBlkSlots, noLastBlkSlots)
import           Pos.Core (FlatSlotId, HasHeaderHash, HeaderHash, genesisHash, HasProtocolConstants,
                           headerHash, slotIdF, unflattenSlotId, HasGenesisHash, HasCoreConfiguration)
import           Pos.Core.Block (Block, BlockHeader)
import           Pos.Crypto (shortHashF)
import           Pos.DB (DBError (..), MonadDB, MonadDBRead (..), RocksBatchOp (..),
                         dbSerializeValue, getHeader)
import           Pos.DB.Class (MonadBlockDBRead, getBlock)
import           Pos.DB.GState.Common (gsGetBi, gsPutBi)
import           Pos.Util.Chrono (OldestFirst (..))
import           Pos.Util.Util (maybeThrow)

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

-- | This function returns 'FlatSlotId's of the blocks whose depth is
-- less than 'blkSecurityParam'.
getLastSlots :: forall m . MonadDBRead m => m LastBlkSlots
getLastSlots =
    maybeThrow (DBMalformed "Last slots not found in the global state DB") =<<
    gsGetBi lastSlotsKey

-- | Retrieves first genesis block hash.
getFirstGenesisBlockHash :: (MonadDBRead m, MonadThrow m, HasGenesisHash) => m HeaderHash
getFirstGenesisBlockHash =
    resolveForwardLink (genesisHash :: HeaderHash) >>=
    maybeThrow (DBMalformed "Can't retrieve genesis block, maybe db is not initialized?")

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

instance HasProtocolConstants => Buildable BlockExtraOp where
    build (AddForwardLink from to) =
        bprint ("AddForwardLink from "%shortHashF%" to "%shortHashF) from to
    build (RemoveForwardLink from) =
        bprint ("RemoveForwardLink from "%shortHashF) from
    build (SetInMainChain flag h) =
        bprint ("SetInMainChain for "%shortHashF%": "%build) h flag
    build (SetLastSlots slots) =
        bprint ("SetLastSlots: "%listJson)
        (map (bprint slotIdF . unflattenSlotId) slots)

instance HasCoreConfiguration => RocksBatchOp BlockExtraOp where
    toBatchOp (AddForwardLink from to) =
        [Rocks.Put (forwardLinkKey from) (dbSerializeValue to)]
    toBatchOp (RemoveForwardLink from) =
        [Rocks.Del $ forwardLinkKey from]
    toBatchOp (SetInMainChain False h) =
        [Rocks.Del $ mainChainKey h]
    toBatchOp (SetInMainChain True h) =
        [Rocks.Put (mainChainKey h) (dbSerializeValue ()) ]
    toBatchOp (SetLastSlots slots) =
        [Rocks.Put lastSlotsKey (dbSerializeValue slots)]

----------------------------------------------------------------------------
-- Loops on forward links
----------------------------------------------------------------------------

foldlUpWhileM
    :: forall a b m r .
    ( MonadDBRead m
    , HasHeaderHash a
    )
    => (HeaderHash -> m (Maybe b)) -- ^ For each header we get b(lund)
    -> a                           -- ^ We start iterating from it
    -> (b -> Int -> Bool)          -- ^ Condition on b and depth
    -> (r -> b -> m r)             -- ^ Conversion function
    -> r                           -- ^ Starting value
    -> m r
foldlUpWhileM getData start condition accM init =
    loadUpWhileDo (headerHash start) 0 init
  where
    loadUpWhileDo :: HeaderHash -> Int -> r -> m r
    loadUpWhileDo curH height !res = getData curH >>= \case
        Nothing -> pure res
        Just someData -> do
            mbNextLink <- resolveForwardLink curH
            if | not (condition someData height) -> pure res
               | Just nextLink <- mbNextLink -> do
                     newRes <- accM res someData
                     loadUpWhileDo nextLink (succ height) newRes
               | otherwise -> accM res someData

-- Loads something from old to new. foldlUpWhileM for (OldestFirst []).
loadUpWhile
    :: forall a b m . (MonadDBRead m, HasHeaderHash a)
    => (HeaderHash -> m (Maybe b))
    -> a
    -> (b -> Int -> Bool)
    -> m (OldestFirst [] b)
loadUpWhile morph start condition = OldestFirst . reverse <$>
    foldlUpWhileM
        morph
        start
        condition
        (\l e -> pure (e : l))
        []

-- | Return hashes loaded up. Basically a forward links traversal.
loadHashesUpWhile
    :: forall m a. (HasHeaderHash a, MonadDBRead m)
    => a
    -> (HeaderHash -> Int -> Bool)
    -> m (OldestFirst [] HeaderHash)
loadHashesUpWhile = loadUpWhile (pure . Just)

-- | Returns headers loaded up.
loadHeadersUpWhile
    :: (MonadBlockDBRead m, HasHeaderHash a)
    => a
    -> (BlockHeader -> Int -> Bool)
    -> m (OldestFirst [] BlockHeader)
loadHeadersUpWhile = loadUpWhile getHeader

-- | Returns blocks loaded up.
loadBlocksUpWhile
    :: (MonadBlockDBRead m, HasHeaderHash a)
    => a
    -> (Block -> Int -> Bool)
    -> m (OldestFirst [] Block)
loadBlocksUpWhile = loadUpWhile getBlock

----------------------------------------------------------------------------
-- Initialization
----------------------------------------------------------------------------

initGStateBlockExtra :: (HasGenesisHash, MonadDB m) => HeaderHash -> m ()
initGStateBlockExtra firstGenesisHash = do
    gsPutBi (mainChainKey firstGenesisHash) ()
    gsPutBi (forwardLinkKey genesisHash) firstGenesisHash
    gsPutBi lastSlotsKey noLastBlkSlots

----------------------------------------------------------------------------
-- Keys
----------------------------------------------------------------------------

forwardLinkKey :: HeaderHash -> ByteString
forwardLinkKey h = "e/fl/" <> serialize' h

mainChainKey :: HeaderHash -> ByteString
mainChainKey h = "e/mc/" <> serialize' h

lastSlotsKey :: ByteString
lastSlotsKey = "e/ls/"
