{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Blockchain listener for Explorer.
-- Callbacks on application and rollback.

module Pos.Explorer.BListener
       ( runExplorerBListener
       , ExplorerBListener
       -- * Instances
       -- ** MonadBListener (ExplorerBListener m)
       -- * Required for tests
       , epochPagedBlocksMap
       -- * Required for migration
       , findEpochMaxPages
       ) where

import           Universum

import           Control.Lens                 (at, non)
import           Control.Monad.Trans.Identity (IdentityT (..))
import           Data.Coerce                  (coerce)
import           Data.List                    ((\\))
import qualified Data.List.NonEmpty           as NE
import qualified Data.Map                     as M
import qualified Ether
import           System.Wlog                  (WithLogger)

import           Pos.Block.BListener          (MonadBListener (..))
import           Pos.Block.Core               (Block, MainBlock, mainBlockSlot,
                                               mainBlockTxPayload)
import           Pos.Block.Types              (Blund)
import           Pos.Core                     (HasConfiguration, HeaderHash, difficultyL,
                                               epochIndexL, getChainDifficulty,
                                               getSlotIndex, headerHash, siSlot)
import           Pos.Crypto                   (withHash)
import           Pos.DB.BatchOp               (SomeBatchOp (..))
import           Pos.DB.Class                 (MonadDBRead)
import           Pos.Explorer.DB              (Epoch, EpochPagedBlocksKey, Page, pageSize,
                                               findEpochMaxPages, numOfLastTxs)
import qualified Pos.Explorer.DB              as DB
import           Pos.Ssc.Class.Helpers        (SscHelpersClass)
import           Pos.Txp                      (Tx, topsortTxs, txpTxs)
import           Pos.Util.Chrono              (NE, NewestFirst (..), OldestFirst (..),
                                               toNewestFirst)



----------------------------------------------------------------------------
-- Declarations
----------------------------------------------------------------------------


data ExplorerBListenerTag

type ExplorerBListener = Ether.TaggedTrans ExplorerBListenerTag IdentityT

-- Kind of... Empty and devoid of meaning...
runExplorerBListener :: ExplorerBListener m a -> m a
runExplorerBListener = coerce

-- Type alias, remove duplication
type MonadBListenerT m ssc =
    ( SscHelpersClass ssc
    , WithLogger m
    , MonadCatch m
    , MonadDBRead m
    , HasConfiguration
    )

-- Explorer implementation for usual node. Combines the operations.
instance ( MonadDBRead m
         , MonadCatch m
         , WithLogger m
         , HasConfiguration
         )
         => MonadBListener (ExplorerBListener m) where
    onApplyBlocks     blunds = onApplyCallGeneral blunds
    onRollbackBlocks  blunds = onRollbackCallGeneral blunds


----------------------------------------------------------------------------
-- General calls
----------------------------------------------------------------------------


onApplyCallGeneral
    :: forall ssc m.
    MonadBListenerT m ssc
    => OldestFirst NE (Blund ssc)
    -> m SomeBatchOp
onApplyCallGeneral    blunds = do
    epochBlocks <- onApplyEpochBlocksExplorer blunds
    pageBlocks  <- onApplyPageBlocksExplorer blunds
    lastTxs     <- onApplyLastTxsExplorer blunds
    pure $ SomeBatchOp [epochBlocks, pageBlocks, lastTxs]


onRollbackCallGeneral
    :: forall ssc m.
    MonadBListenerT m ssc
    => NewestFirst NE (Blund ssc)
    -> m SomeBatchOp
onRollbackCallGeneral blunds = do
    epochBlocks <- onRollbackEpochBlocksExplorer blunds
    pageBlocks  <- onRollbackPageBlocksExplorer blunds
    lastTxs     <- onRollbackLastTxsExplorer blunds
    pure $ SomeBatchOp [epochBlocks, pageBlocks, lastTxs]


----------------------------------------------------------------------------
-- Function calls
----------------------------------------------------------------------------

-- For @EpochBlocks@
onApplyEpochBlocksExplorer
    :: forall ssc m.
    MonadBListenerT m ssc
    => OldestFirst NE (Blund ssc)
    -> m SomeBatchOp
onApplyEpochBlocksExplorer blunds = onApplyEpochPagedBlocks blunds


-- For @PageBlocks@
onApplyPageBlocksExplorer
    :: forall ssc m.
    MonadBListenerT m ssc
    => OldestFirst NE (Blund ssc)
    -> m SomeBatchOp
onApplyPageBlocksExplorer blunds = onApplyKeyBlocksGeneral blunds pageBlocksMap


-- For last transactions, @Tx@
onApplyLastTxsExplorer
    :: forall ssc m.
    MonadBListenerT m ssc
    => OldestFirst NE (Blund ssc)
    -> m SomeBatchOp
onApplyLastTxsExplorer blunds = generalLastTxsExplorer blocksNE getTopTxsDiff
  where
    -- Get the top transactions by pulling the old top transactions and adding
    -- new transactions. After we append them, take the last N transactions and
    -- we have our top transactions.
    getTopTxsDiff
        :: OldTxs
        -> NewTxs
        -> [Tx]
    getTopTxsDiff oldTxs newTxs = take numOfLastTxs reversedCombined
      where
        reversedCombined :: [Tx]
        reversedCombined = reverse $ getOldTxs oldTxs ++ getNewTxs newTxs

    blocksNE :: NE (Block ssc)
    blocksNE = fst <$> getOldestFirst blunds


----------------------------------------------------------------------------
-- Rollback
----------------------------------------------------------------------------


-- For @EpochBlocks@
onRollbackEpochBlocksExplorer
    :: forall ssc m.
    MonadBListenerT m ssc
    => NewestFirst NE (Blund ssc)
    -> m SomeBatchOp
onRollbackEpochBlocksExplorer blunds = onRollbackEpochPagedBlocks blunds


-- For @PageBlocks@
onRollbackPageBlocksExplorer
    :: forall ssc m.
    MonadBListenerT m ssc
    => NewestFirst NE (Blund ssc)
    -> m SomeBatchOp
onRollbackPageBlocksExplorer blunds = onRollbackGeneralBlocks blunds pageBlocksMap


-- For last transactions, @Tx@
onRollbackLastTxsExplorer
    :: forall ssc m.
    MonadBListenerT m ssc
    => NewestFirst NE (Blund ssc)
    -> m SomeBatchOp
onRollbackLastTxsExplorer blunds = generalLastTxsExplorer blocksNE getTopTxsDiff
  where
    -- Get the top transactions by pulling the old top transactions and removing
    -- new transactions. After we remove them, what remains are the new top
    -- transactions.
    getTopTxsDiff
        :: OldTxs
        -> NewTxs
        -> [Tx]
    getTopTxsDiff oldTxs newTxs = getOldTxs oldTxs \\ getNewTxs newTxs

    blocksNE :: NE (Block ssc)
    blocksNE = fst <$> getNewestFirst blunds


----------------------------------------------------------------------------
-- Common
----------------------------------------------------------------------------

-- Return a map from @Page@ to @HeaderHash@es for all non-empty blocks.
pageBlocksMap
    :: forall ssc. (HasConfiguration, SscHelpersClass ssc)
    => NE (Block ssc)
    -> M.Map Page [HeaderHash]
pageBlocksMap neBlocks = blocksPages
  where
    -- | Finally, create a map from @Page@ to a list of @HeaderHash@. In other words,
    -- group all blocks @HeaderHash@ to corresponding @Page@.
    blocksPages :: M.Map Page [HeaderHash]
    blocksPages =
        M.fromListWith (++) [ (k, [v]) | (k, v) <- createPagedHeaderHashesPair blocks]
      where
        -- | Get blocks as list.
        blocks :: [Block ssc]
        blocks = NE.toList neBlocks

-- | Creates paged @HeaderHash@es @SlotId@ pair.
-- TODO(ks): Yes, we can extract both these functions in one common function for paging.
createPagedHeaderHashesSlotIdPair
    :: forall ssc. (HasConfiguration, SscHelpersClass ssc)
    => [Block ssc]
    -> [(Page, HeaderHash)]
createPagedHeaderHashesSlotIdPair blocks = blockIndexBlock
  where
    -- Zip the @Page@ number with the @HeaderHash@ we can use to retrieve block.
    blockIndexBlock :: [(Page, HeaderHash)]
    blockIndexBlock = blockPages `zip` blocksHHs
      where
        blocksHHs :: [HeaderHash]
        blocksHHs = headerHash <$> blocks

        -- | Calculate which block index goes into which page
        blockPages :: [Page]
        blockPages = getCurrentPage <$> blockIndexes
          where
            -- | Get the page the index belongs to.
            getCurrentPage :: Int -> Page
            getCurrentPage blockIndex = ((blockIndex - 1) `div` pageSize) + 1

            -- | Get the blocks index numbers.
            blockIndexes :: [Int]
            blockIndexes = getBlockIndex <$> blocks
              where
                -- | Get the block index number. We start with the the index 1 for the
                -- gensis block and add 1 for the main blocks since they start with 1
                -- as well.
                getBlockIndex :: (Block ssc) -> Int
                getBlockIndex (Left _)      = 1
                getBlockIndex (Right block) =
                    fromIntegral $ (+1) $ getSlotIndex $ siSlot $ block ^. mainBlockSlot

-- | Creates paged @HeaderHash@es pair.
-- TODO(ks): Yes, we can extract both these functions in one common function for paging.
createPagedHeaderHashesPair
    :: forall ssc. (HasConfiguration, SscHelpersClass ssc)
    => [Block ssc]
    -> [(Page, HeaderHash)]
createPagedHeaderHashesPair blocks = blockIndexBlock
  where
      -- Zip the @Page@ number with the @HeaderHash@ we can use to retrieve block.
    blockIndexBlock :: [(Page, HeaderHash)]
    blockIndexBlock = blockPages `zip` blocksHHs
      where
        blocksHHs :: [HeaderHash]
        blocksHHs = headerHash <$> blocks

        -- | Calculate which block index goes into which page
        blockPages :: [Page]
        blockPages = getCurrentPage <$> blockIndexes
          where
            -- | Get the page the index belongs to.
            getCurrentPage :: Int -> Page
            getCurrentPage blockIndex = ((blockIndex - 1) `div` pageSize) + 1

            -- | Get the blocks index numbers.
            blockIndexes :: [Int]
            blockIndexes = getBlockIndex <$> blocks
              where
                -- | Get the block index number.
                getBlockIndex :: (Block ssc) -> Int
                getBlockIndex block =
                    fromIntegral $ getChainDifficulty $ block ^. difficultyL


-- So the parameters can be type checked.
newtype PrevKBlocks  k = PrevKBlocks { getPrevKBlocks :: M.Map k [HeaderHash] }
newtype NewKBlocks   k = NewKBlocks { getNewKBlocks :: M.Map k [HeaderHash] }


-- The result is the map that contains diffed blocks from the keys.
rollbackedBlocks
    :: forall a
     . (Ord a)
    => [a]
    -> PrevKBlocks a
    -> NewKBlocks a
    -> M.Map a [HeaderHash]
rollbackedBlocks keys' oldMap' newMap' = M.unions rolledbackKeyMaps
  where
    rolledbackKeyMaps = [ rollbackedKey key oldMap' newMap' | key <- keys' ]

    -- From a single key, retrieve blocks and return their difference
    rollbackedKey
        :: a
        -> PrevKBlocks a
        -> NewKBlocks a
        -> M.Map a [HeaderHash]
    rollbackedKey key oldMap newMap = elementsExist
      where
        newBlocks' :: [HeaderHash]
        newBlocks' = getNewKBlocks newMap ^. at key . non []

        existingBlocks :: [HeaderHash]
        existingBlocks = getPrevKBlocks oldMap ^. at key . non []

        elementsExist :: M.Map a [HeaderHash]
        elementsExist = M.singleton key finalBlocks
          where
            -- Rollback the new blocks, remove them from the collection
            finalBlocks :: [HeaderHash]
            finalBlocks = existingBlocks \\ newBlocks'


-- The repetitions can be extracted
class (Ord k) => KeyBlocksOperation k where
    putKeyBlocksF :: (k -> [HeaderHash] -> DB.ExplorerOp)
    getKeyBlocksF :: (MonadDBRead m) => (k -> m (Maybe [HeaderHash]))

instance KeyBlocksOperation (Epoch, Page) where
    putKeyBlocksF (epoch, page) = DB.PutEpochBlocks epoch page
    getKeyBlocksF (epoch, page) = DB.getEpochBlocks epoch page

instance KeyBlocksOperation Page where
    putKeyBlocksF = DB.PutPageBlocks
    getKeyBlocksF = DB.getPageBlocks


-- For each (k, [v]) pair, create a database operation.
putKeysBlocks
    :: forall k
     . (KeyBlocksOperation k)
    => M.Map k [HeaderHash]
    -> [DB.ExplorerOp]
putKeysBlocks keysBlocks = putKeyBlocks <$> M.toList keysBlocks
  where
    putKeyBlocks
        :: (k, [HeaderHash])
        -> DB.ExplorerOp
    putKeyBlocks keyBlocks = putKeyBlocksF key uniqueBlocks
      where
        key           = keyBlocks ^. _1
        blocks        = keyBlocks ^. _2
        uniqueBlocks  = ordNub blocks


-- Get exisiting key blocks paired with the key.
getExistingBlocks
    :: forall k m. (MonadDBRead m, KeyBlocksOperation k)
    => [k]
    -> m (M.Map k [HeaderHash])
getExistingBlocks keys = do
    keyBlocks <- traverse getExistingKeyBlocks keys
    pure $ M.unions keyBlocks
  where
    -- Get exisiting key blocks paired with the key. If there are no
    -- saved blocks on the key return an empty list.
    getExistingKeyBlocks
        :: k
        -> m (M.Map k [HeaderHash])
    getExistingKeyBlocks key = do
        mKeyBlocks    <- getKeyBlocksF key
        let keyBlocks  = fromMaybe [] mKeyBlocks
        pure $ M.singleton key keyBlocks


-- A general @Key@ @Block@ database application for the apply call.
{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}
onApplyKeyBlocksGeneral
    :: forall k ssc m.
    (MonadBListenerT m ssc, KeyBlocksOperation k)
    => OldestFirst NE (Blund ssc)
    -> (NE (Block ssc) -> M.Map k [HeaderHash])
    -> m SomeBatchOp
onApplyKeyBlocksGeneral blunds newBlocksMapF = do

    -- Get existing @HeaderHash@es from the keys so we can merge them.
    existingBlocks <- getExistingBlocks keys

    -- Merge the new and the old
    let mergedBlocksMap = M.unionWith (<>) existingBlocks newBlocks

    -- Create database operation
    let mergedBlocks = putKeysBlocks mergedBlocksMap

    -- In the end make sure we place this under @SomeBatchOp@ in order to
    -- preserve atomicity.
    pure $ SomeBatchOp mergedBlocks
  where

    keys :: [k]
    keys = M.keys newBlocks

    newBlocks :: M.Map k [HeaderHash]
    newBlocks = newBlocksMapF blocksNE

    blocksNE :: NE (Block ssc)
    blocksNE = fst <$> getNewestFirst blocksNewF

    blocksNewF :: NewestFirst NE (Blund ssc)
    blocksNewF = toNewestFirst blunds


-- A general @Key@ @Block@ database application for the rollback call.
{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}
onRollbackGeneralBlocks
    :: forall k ssc m.
    (MonadBListenerT m ssc, KeyBlocksOperation k)
    => NewestFirst NE (Blund ssc)
    -> (NE (Block ssc) -> M.Map k [HeaderHash])
    -> m SomeBatchOp
onRollbackGeneralBlocks blunds newBlocksMapF = do

    -- Get existing @HeaderHash@es from the keys so we can merge them.
    existingBlocks <- getExistingBlocks keys
    let prevKeyBlocks = PrevKBlocks existingBlocks
    let newKeyBlocks  = NewKBlocks  newBlocks

    -- Diffrence between the new and the old
    let mergedBlocksMap = rollbackedBlocks keys prevKeyBlocks newKeyBlocks

    -- Create database operation
    let mergedBlocks = putKeysBlocks mergedBlocksMap

    -- In the end make sure we place this under @SomeBatchOp@ in order to
    -- preserve atomicity.
    pure $ SomeBatchOp mergedBlocks
  where

    keys :: [k]
    keys = M.keys newBlocks

    newBlocks :: M.Map k [HeaderHash]
    newBlocks = newBlocksMapF blocksNE

    blocksNE :: NE (Block ssc)
    blocksNE = fst <$> getNewestFirst blunds

-- Wrappers so I don't mess up the parameter order
newtype OldTxs = OldTxs { getOldTxs :: [Tx] }
newtype NewTxs = NewTxs { getNewTxs :: [Tx] }

-- If you give me non-empty blocks that contain transactions and a way to
-- combine old and new transactions I will return you an
-- atomic database operation.
generalLastTxsExplorer
    :: forall ssc m .
    MonadBListenerT m ssc
    => NE (Block ssc)
    -> (OldTxs -> NewTxs -> [Tx])
    -> m SomeBatchOp
generalLastTxsExplorer blocksNE getTopTxsDiff = do
    let newTxs       = NewTxs mainBlocksTxs

    mPrevTopTxs     <- DB.getLastTransactions
    let prevTopTxs   = OldTxs $ fromMaybe [] mPrevTopTxs

    let newTopTxs    = getTopTxsDiff prevTopTxs newTxs

    -- Create database operation
    let mergedTopTxs = DB.PutLastTxs newTopTxs

    -- In the end make sure we place this under @SomeBatchOp@ in order to
    -- preserve atomicity.
    pure $ SomeBatchOp mergedTopTxs

  where

    mainBlocksTxs :: [Tx]
    mainBlocksTxs = concat $ catMaybes mMainBlocksTxs

    mMainBlocksTxs :: [Maybe [Tx]]
    mMainBlocksTxs = blockTxs <$> mainBlocks

    blockTxs :: MainBlock ssc -> Maybe [Tx]
    blockTxs mb = topsortTxs withHash $ toList $ mb ^. mainBlockTxPayload . txpTxs

    mainBlocks :: [MainBlock ssc]
    mainBlocks = rights blocks

    blocks :: [Block ssc]
    blocks = NE.toList blocksNE

----------------------------------------------------------------------------
-- Epoch paged map
-- TODO(ks): I know, I know, it could be more general, but let's worry about
-- that when we have more time.
----------------------------------------------------------------------------

-- Return a map from @Epoch@ to @HeaderHash@es for all non-empty blocks.
epochPagedBlocksMap
    :: forall ssc. (HasConfiguration, SscHelpersClass ssc)
    => NE (Block ssc)
    -> M.Map EpochPagedBlocksKey [HeaderHash]
epochPagedBlocksMap neBlocks = getPagedEpochHeaderHashesMap
  where
    -- | Get a map from the "composite key" of @Epoch@ and @Page@ that return a list of
    -- @HeaderHash@es on that @Page@ in that @Epoch@.
    getPagedEpochHeaderHashesMap :: M.Map EpochPagedBlocksKey [HeaderHash]
    getPagedEpochHeaderHashesMap =
        M.fromListWith (++) [ (k, [v]) | (k, v) <- epochPagedHeaderHashes]
      where

        epochPagedHeaderHashes :: [(EpochPagedBlocksKey, HeaderHash)]
        epochPagedHeaderHashes = concat $ createEpochPagedHeaderHashes <$> getAllEpochs
          where
            getAllEpochs :: [Epoch]
            getAllEpochs = M.keys blocksEpochs

        createEpochPagedHeaderHashes :: Epoch -> [(EpochPagedBlocksKey, HeaderHash)]
        createEpochPagedHeaderHashes epoch = createEpochPageHHAssoc epoch <$> pageBlocks
          where
            -- | Get @HeaderHash@es grouped by @Page@ inside an @Epoch@.
            pageBlocks :: [(Page, HeaderHash)]
            pageBlocks = createPagedHeaderHashesSlotIdPair $ getEpochHeaderHashes epoch
              where
                getEpochHeaderHashes
                    :: Epoch
                    -> [Block ssc]
                getEpochHeaderHashes epochKey = case blocksEpochs ^. at epochKey of
                    Nothing      -> []
                    Just blocks' -> blocks'

            -- | Just add @Epoch@ to the @(Page, HeaderHash)@ pair, so we can group
            -- @Epoch@ and @Page@ and reuse it like a composite key.
            createEpochPageHHAssoc
                :: Epoch
                -> (Page, HeaderHash)
                -> (EpochPagedBlocksKey, HeaderHash)
            createEpochPageHHAssoc epoch' pageBlock =
                ((epoch', pageBlock ^. _1), pageBlock ^. _2)

    -- | Finally, create a map from @Epoch@ to a list of @HeaderHash@. In other words,
    -- group all blocks @HeaderHash@ to corresponding @Epoch@.
    blocksEpochs :: M.Map Epoch [Block ssc]
    blocksEpochs = M.fromListWith (++) [ (k, [v]) | (k, v) <- blockEpochs]
      where
        blockEpochs :: [(Epoch, Block ssc)]
        blockEpochs = getBlockEpoch <$> blocks
          where
            getBlockEpoch :: (Block ssc) -> (Epoch, Block ssc)
            getBlockEpoch block = (block ^. epochIndexL, block)

    -- | Get blocks as list.
    blocks :: [Block ssc]
    blocks = NE.toList neBlocks

-- A general @Key@ @Block@ database application for the apply call.
{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}
onApplyEpochPagedBlocks
    :: forall ssc m.
    (MonadBListenerT m ssc, KeyBlocksOperation EpochPagedBlocksKey)
    => OldestFirst NE (Blund ssc)
    -> m SomeBatchOp
onApplyEpochPagedBlocks blunds = do

    -- Get existing @HeaderHash@es from the keys so we can merge them.
    existingBlocks <- getExistingBlocks keys

    -- Merge the new and the old
    let mergedBlocksMap = M.unionWith (<>) existingBlocks newBlocks

    -- Find the maximum page number for each @Epoch@.
    let maxPagesEpochBlocks = findEpochMaxPages mergedBlocksMap
    let epochMaxPages = [ DB.PutEpochPages epoch maxPageNumber | (epoch, maxPageNumber) <- maxPagesEpochBlocks]

    -- Create database operation
    let mergedBlocks = putKeysBlocks mergedBlocksMap

    -- In the end make sure we place this under @SomeBatchOp@ in order to
    -- preserve atomicity.
    pure $ SomeBatchOp [mergedBlocks, epochMaxPages]
  where

    keys :: [EpochPagedBlocksKey]
    keys = M.keys newBlocks

    newBlocks :: M.Map EpochPagedBlocksKey [HeaderHash]
    newBlocks = epochPagedBlocksMap blocksNE

    blocksNE :: NE (Block ssc)
    blocksNE = fst <$> getNewestFirst blocksNewF

    blocksNewF :: NewestFirst NE (Blund ssc)
    blocksNewF = toNewestFirst blunds


-- A general @Key@ @Block@ database application for the rollback call.
{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}
onRollbackEpochPagedBlocks
    :: forall ssc m.
    (MonadBListenerT m ssc, KeyBlocksOperation EpochPagedBlocksKey)
    => NewestFirst NE (Blund ssc)
    -> m SomeBatchOp
onRollbackEpochPagedBlocks blunds = do

    -- Get existing @HeaderHash@es from the keys so we can merge them.
    existingBlocks <- getExistingBlocks keys
    let prevKeyBlocks = PrevKBlocks existingBlocks
    let newKeyBlocks  = NewKBlocks  newBlocks

    -- Diffrence between the new and the old
    let mergedBlocksMap = rollbackedBlocks keys prevKeyBlocks newKeyBlocks

    -- Create database operation
    let mergedBlocks = putKeysBlocks mergedBlocksMap

    -- In the end make sure we place this under @SomeBatchOp@ in order to
    -- preserve atomicity.
    pure $ SomeBatchOp mergedBlocks
  where

    keys :: [EpochPagedBlocksKey]
    keys = M.keys newBlocks

    newBlocks :: M.Map EpochPagedBlocksKey [HeaderHash]
    newBlocks = epochPagedBlocksMap blocksNE

    blocksNE :: NE (Block ssc)
    blocksNE = fst <$> getNewestFirst blunds