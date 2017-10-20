{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}

-- | Blockchain listener for Explorer.
-- Callbacks on application and rollback.

module Pos.Explorer.BListener
       ( runExplorerBListener
       , ExplorerBListener
       -- * Instances
       -- ** MonadBListener (ExplorerBListener m)
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
import           Pos.Block.Core               (Block, MainBlock, mainBlockTxPayload)
import           Pos.Block.Types              (Blund)
import           Pos.Core                     (HasConfiguration, HeaderHash, difficultyL,
                                               epochIndexL, getChainDifficulty,
                                               headerHash)
import           Pos.Crypto                   (withHash)
import           Pos.DB.BatchOp               (SomeBatchOp (..))
import           Pos.DB.Class                 (MonadDBRead)
import           Pos.Explorer.DB              (Epoch, Page, numOfLastTxs)
import qualified Pos.Explorer.DB              as DB
import           Pos.Txp                      (Tx, topsortTxs, txpTxs)
import           Pos.Util.Chrono              (NE, NewestFirst (..), OldestFirst (..),
                                               toNewestFirst)
import           Pos.Util.Util                (inAssertMode)

----------------------------------------------------------------------------
-- Declarations
----------------------------------------------------------------------------


data ExplorerBListenerTag

type ExplorerBListener = Ether.TaggedTrans ExplorerBListenerTag IdentityT

-- Kind of... Empty and devoid of meaning...
runExplorerBListener :: ExplorerBListener m a -> m a
runExplorerBListener = coerce

-- Type alias, remove duplication
type MonadBListenerT m =
    ( WithLogger m
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
    :: MonadBListenerT m
    => OldestFirst NE Blund -> m SomeBatchOp
onApplyCallGeneral    blunds = do
    epochBlocks <- onApplyEpochBlocksExplorer blunds
    pageBlocks  <- onApplyPageBlocksExplorer blunds
    lastTxs     <- onApplyLastTxsExplorer blunds
    inAssertMode DB.sanityCheckBalances
    pure $ SomeBatchOp [epochBlocks, pageBlocks, lastTxs]


onRollbackCallGeneral
    :: MonadBListenerT m
    => NewestFirst NE Blund -> m SomeBatchOp
onRollbackCallGeneral blunds = do
    epochBlocks <- onRollbackEpochBlocksExplorer blunds
    pageBlocks  <- onRollbackPageBlocksExplorer blunds
    lastTxs     <- onRollbackLastTxsExplorer blunds
    inAssertMode DB.sanityCheckBalances
    pure $ SomeBatchOp [epochBlocks, pageBlocks, lastTxs]


----------------------------------------------------------------------------
-- Function calls
----------------------------------------------------------------------------


-- For @EpochBlocks@
onApplyEpochBlocksExplorer
    :: MonadBListenerT m
    => OldestFirst NE Blund
    -> m SomeBatchOp
onApplyEpochBlocksExplorer blunds = onApplyKeyBlocksGeneral blunds epochBlocksMap


-- For @PageBlocks@
onApplyPageBlocksExplorer
    :: MonadBListenerT m
    => OldestFirst NE Blund
    -> m SomeBatchOp
onApplyPageBlocksExplorer blunds = onApplyKeyBlocksGeneral blunds pageBlocksMap


-- For last transactions, @Tx@
onApplyLastTxsExplorer
    :: MonadBListenerT m
    => OldestFirst NE Blund
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

    blocksNE :: NE Block
    blocksNE = fst <$> getOldestFirst blunds


----------------------------------------------------------------------------
-- Rollback
----------------------------------------------------------------------------


-- For @EpochBlocks@
onRollbackEpochBlocksExplorer
    :: MonadBListenerT m
    => NewestFirst NE Blund -> m SomeBatchOp
onRollbackEpochBlocksExplorer blunds = onRollbackGeneralBlocks blunds epochBlocksMap


-- For @PageBlocks@
onRollbackPageBlocksExplorer
    :: MonadBListenerT m
    => NewestFirst NE Blund -> m SomeBatchOp
onRollbackPageBlocksExplorer blunds = onRollbackGeneralBlocks blunds pageBlocksMap


-- For last transactions, @Tx@
onRollbackLastTxsExplorer
    :: MonadBListenerT m
    => NewestFirst NE Blund -> m SomeBatchOp
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

    blocksNE :: NE Block
    blocksNE = fst <$> getNewestFirst blunds


----------------------------------------------------------------------------
-- Common
----------------------------------------------------------------------------


-- Return a map from @Epoch@ to @HeaderHash@es for all non-empty blocks.
epochBlocksMap
    :: HasConfiguration
    => NE Block
    -> M.Map Epoch [HeaderHash]
epochBlocksMap neBlocks = blocksEpochs
  where

    blocksEpochs :: M.Map Epoch [HeaderHash]
    blocksEpochs = M.fromListWith (++) [ (k, [v]) | (k, v) <- blockEpochBlocks]

    blockEpochBlocks :: [(Epoch, HeaderHash)]
    blockEpochBlocks = blockEpochs `zip` blocksHHs
      where
        blocksHHs :: [HeaderHash]
        blocksHHs = headerHash <$> blocks

        blockEpochs :: [Epoch]
        blockEpochs = getBlockEpoch <$> blocks

    getBlockEpoch :: Block -> Epoch
    getBlockEpoch block = block ^. epochIndexL

    blocks :: [Block]
    blocks = NE.toList neBlocks


-- Return a map from @Page@ to @HeaderHash@es for all non-empty blocks.
pageBlocksMap
    :: HasConfiguration
    => NE Block
    -> M.Map Page [HeaderHash]
pageBlocksMap neBlocks = blocksPages
  where

    blocksPages :: M.Map Page [HeaderHash]
    blocksPages = M.fromListWith (++) [ (k, [v]) | (k, v) <- blockIndexBlock]

    blockIndexBlock :: [(Page, HeaderHash)]
    blockIndexBlock = blockPages `zip` blocksHHs
      where
        blocksHHs :: [HeaderHash]
        blocksHHs = headerHash <$> blocks

        blockPages :: [Page]
        blockPages = getCurrentPage <$> blockIndexes

        getCurrentPage :: Int -> Page
        getCurrentPage blockIndex = ((blockIndex - 1) `div` pageSize) + 1

        pageSize :: Int
        pageSize = 10

    blockIndexes :: [Int]
    blockIndexes = getBlockIndex <$> blocks

    getBlockIndex :: Block -> Int
    getBlockIndex block = fromIntegral $ getChainDifficulty $ block ^. difficultyL

    blocks :: [Block]
    blocks = NE.toList neBlocks


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

instance KeyBlocksOperation Epoch where
    putKeyBlocksF = DB.PutEpochBlocks
    getKeyBlocksF = DB.getEpochBlocks

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
    :: forall k m.
    (MonadBListenerT m, KeyBlocksOperation k)
    => OldestFirst NE Blund
    -> (NE Block -> M.Map k [HeaderHash])
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

    blocksNE :: NE Block
    blocksNE = fst <$> getNewestFirst blocksNewF

    blocksNewF :: NewestFirst NE Blund
    blocksNewF = toNewestFirst blunds


-- A general @Key@ @Block@ database application for the rollback call.
{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}
onRollbackGeneralBlocks
    :: forall k m.
    (MonadBListenerT m, KeyBlocksOperation k)
    => NewestFirst NE Blund
    -> (NE Block -> M.Map k [HeaderHash])
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

    blocksNE :: NE Block
    blocksNE = fst <$> getNewestFirst blunds

-- Wrappers so I don't mess up the parameter order
newtype OldTxs = OldTxs { getOldTxs :: [Tx] }
newtype NewTxs = NewTxs { getNewTxs :: [Tx] }

-- If you give me non-empty blocks that contain transactions and a way to
-- combine old and new transactions I will return you an
-- atomic database operation.
generalLastTxsExplorer
    :: MonadBListenerT m
    => NE Block
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

    blockTxs :: MainBlock -> Maybe [Tx]
    blockTxs mb = topsortTxs withHash $ toList $ mb ^. mainBlockTxPayload . txpTxs

    mainBlocks :: [MainBlock]
    mainBlocks = rights blocks

    blocks :: [Block]
    blocks = NE.toList blocksNE
