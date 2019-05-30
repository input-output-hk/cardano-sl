-- | Extra information for blocks.
--   * Forward links.
--   * InMainChain flags.
--   * Slots of the last 'blkSecurityParam' (at most) blocks
--     (for chain quality check).

module Pos.DB.Block.GState.BlockExtra
       ( resolveForwardLink
       , isBlockInMainChain
       , calcLastBlkSlots
       , upgradeLastSlotsVersion
       , getFirstGenesisBlockHash
       , BlockExtraOp (..)
       , buildBlockExtraOp
       , foldlUpWhileM
       , loadHashesUpWhile
       , loadHeadersUpWhile
       , loadBlocksUpWhile
       , initGStateBlockExtra
       , streamBlocks
       ) where

import           Universum hiding (init)

import           Data.Conduit (ConduitT, yield)

import qualified Database.RocksDB as Rocks
import           Formatting (Format, bprint, build, later, (%))
import           Serokell.Util.Text (listJson)

import           Pos.Binary.Class (serialize')
import           Pos.Chain.Block (Block, BlockHeader (..), HasHeaderHash,
                     HeaderHash, LastBlkSlots, LastSlotInfo (..), headerHash,
                     mainHeaderLeaderKey, prevBlockL)
import qualified Pos.Chain.Block.Slog.LastBlkSlots as LastBlkSlots
import           Pos.Chain.Genesis (GenesisHash (..), configBlkSecurityParam,
                     configEpochSlots)
import qualified Pos.Chain.Genesis as Genesis
import           Pos.Core (BlockCount (..), FlatSlotId, SlotCount, addressHash,
                     flattenEpochOrSlot, getEpochOrSlot, slotIdF,
                     unflattenSlotId)
import           Pos.Core.Chrono (OldestFirst (..))
import           Pos.Crypto (PublicKey, shortHashF)
import           Pos.DB (DBError (..), MonadDB, MonadDBRead (..),
                     RocksBatchOp (..), getHeader, getTipHeader, gsDelete)
import           Pos.DB.Class (MonadBlockDBRead, SerializedBlock, getBlock)
import           Pos.DB.GState.Common (gsGetBi, gsPutBi)
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
calcLastBlkSlots :: forall m . MonadDBRead m => Genesis.Config -> m LastBlkSlots
calcLastBlkSlots genesisConfig = do
    xs <- calcLastSlotInfo genesisConfig
    pure $ LastBlkSlots.fromList (fromIntegral . getBlockCount $ configBlkSecurityParam genesisConfig) xs

-- | This function acts as a one time conversion from version 1 to version 2
-- of the `LastBlkSlots` data type.
upgradeLastSlotsVersion :: forall m . MonadDB m => Genesis.Config -> m ()
upgradeLastSlotsVersion _ = do
    -- Delete both the old keys, because this is no longer stored in the DB.
    gsDelete oldLastSlotsKey
    gsDelete lastSlotsKey2
  where
    -- The old DB keys that we no longer use.
    oldLastSlotsKey :: ByteString
    oldLastSlotsKey = "e/ls/"

    lastSlotsKey2 :: ByteString
    lastSlotsKey2 = "e/ls2/"


-- Get the 'LastSlotInfo' data for the last 'k' (security paramenter) starting
-- at the current blockchain tip.
calcLastSlotInfo :: MonadDBRead m => Genesis.Config -> m (OldestFirst [] LastSlotInfo)
calcLastSlotInfo genesisConfig = do
    th <- getTipHeader
    let thfsid = flattenEpochOrSlot (configEpochSlots genesisConfig) $ getEpochOrSlot th
        lastfsid =
            if thfsid <= fromIntegral (configBlkSecurityParam genesisConfig - 1)
                then 0
                else thfsid - fromIntegral (configBlkSecurityParam genesisConfig - 1)
    OldestFirst <$> convert th lastfsid []
  where
    convert :: MonadDBRead m => BlockHeader -> FlatSlotId -> [LastSlotInfo] -> m [LastSlotInfo]
    convert bh lastFsid !acc = do
        let bhFsid = flattenEpochOrSlot (configEpochSlots genesisConfig) $ getEpochOrSlot bh
        if bhFsid < lastFsid
            then pure acc
            else do
                let ys = case leaderKey bh of
                            Nothing -> acc
                            Just lk -> LastSlotInfo bhFsid (addressHash lk) : acc
                mnbh <- getHeader $ view prevBlockL bh
                case mnbh of
                    Nothing  -> pure acc
                    Just nbh -> convert nbh lastFsid ys

    leaderKey :: BlockHeader -> Maybe PublicKey
    leaderKey = \case
        BlockHeaderGenesis _ -> Nothing
        BlockHeaderMain bhm -> Just $ view mainHeaderLeaderKey bhm

-- | Retrieves first genesis block hash.
getFirstGenesisBlockHash :: MonadDBRead m => GenesisHash -> m HeaderHash
getFirstGenesisBlockHash genesisHash =
    resolveForwardLink (getGenesisHash genesisHash :: HeaderHash) >>=
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
    | SetLastSlots LastBlkSlots
      -- ^ Updates list of slots for last blocks.
    deriving (Show)

buildBlockExtraOp :: SlotCount -> Format r (BlockExtraOp -> r)
buildBlockExtraOp epochSlots = later build'
  where
    build' (AddForwardLink from to) =
        bprint ("AddForwardLink from "%shortHashF%" to "%shortHashF) from to
    build' (RemoveForwardLink from) =
        bprint ("RemoveForwardLink from "%shortHashF) from
    build' (SetInMainChain flag h) =
        bprint ("SetInMainChain for "%shortHashF%": "%build) h flag
    build' (SetLastSlots slots) =
        bprint ("SetLastSlots: "%listJson)
        (map (bprint slotIdF . unflattenSlotId epochSlots . lsiFlatSlotId) $ LastBlkSlots.getList slots)

instance RocksBatchOp BlockExtraOp where
    toBatchOp (AddForwardLink from to) =
        [Rocks.Put (forwardLinkKey from) (serialize' to)]
    toBatchOp (RemoveForwardLink from) =
        [Rocks.Del $ forwardLinkKey from]
    toBatchOp (SetInMainChain False h) =
        [Rocks.Del $ mainChainKey h]
    toBatchOp (SetInMainChain True h) =
        [Rocks.Put (mainChainKey h) (serialize' ()) ]
    toBatchOp (SetLastSlots _) =
        [{- No longer store this in the DB -}]

----------------------------------------------------------------------------
-- Loops on forward links
----------------------------------------------------------------------------

-- | Creates a Producer for blocks from a given HeaderHash, exclusive: the
-- block for that hash is not produced, its child is the first thing produced.
streamBlocks
    :: ( Monad m )
    => (HeaderHash -> m (Maybe SerializedBlock))
    -> (HeaderHash -> m (Maybe HeaderHash))
    -> HeaderHash
    -> ConduitT () SerializedBlock m ()
streamBlocks loadBlock forwardLink base = do
    mFirst <- lift $ forwardLink base
    maybe (pure ()) loop mFirst
  where
    loop hhash = do
        mb <- lift $ loadBlock hhash
        case mb of
            Nothing -> pure ()
            Just block -> do
                yield block
                mNext <- lift $ forwardLink hhash
                case mNext of
                    Nothing     -> pure ()
                    Just hhash' -> loop  hhash'

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
    => GenesisHash
    -> a
    -> (Block -> Int -> Bool)
    -> m (OldestFirst [] Block)
loadBlocksUpWhile genesisHash = loadUpWhile (getBlock genesisHash)

----------------------------------------------------------------------------
-- Initialization
----------------------------------------------------------------------------

initGStateBlockExtra :: MonadDB m => GenesisHash -> HeaderHash -> m ()
initGStateBlockExtra genesisHash firstGenesisHash = do
    gsPutBi (mainChainKey firstGenesisHash) ()
    gsPutBi (forwardLinkKey $ getGenesisHash genesisHash) firstGenesisHash

----------------------------------------------------------------------------
-- Keys
----------------------------------------------------------------------------

forwardLinkKey :: HeaderHash -> ByteString
forwardLinkKey h = "e/fl/" <> serialize' h

mainChainKey :: HeaderHash -> ByteString
mainChainKey h = "e/mc/" <> serialize' h
