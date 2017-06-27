{-# LANGUAGE ScopedTypeVariables #-}

-- | Blockchain listener for Explorer.
-- Callbacks on application and rollback.

module Pos.Explorer.BListener
       ( runExplorerBListener 
       -- * Instances
       -- ** MonadBListener (ExplorerBListener m)
       ) where

import           Universum

import           Control.Monad.Trans.Identity (IdentityT (..))
import           Data.Coerce                  (coerce)
import           Data.List                    (nub)
import qualified Data.List.NonEmpty           as NE
import qualified Data.Map                     as M
import qualified Ether
import           System.Wlog                  (WithLogger)

import           Mockable                     (MonadMockable)
import           Pos.Block.BListener          (MonadBListener (..))
import           Pos.Block.Core               (Block)
import           Pos.Block.Types              (Blund)
import           Pos.Core                     (HeaderHash, difficultyL,
                                               epochIndexL, getChainDifficulty,
                                               headerHash)
import           Pos.DB.BatchOp               (SomeBatchOp (..))
import           Pos.DB.Class                 (MonadDBRead, MonadRealDB)
import           Pos.Explorer.DB              (Page, Epoch)
import qualified Pos.Explorer.DB              as DB
import           Pos.Ssc.Class.Helpers        (SscHelpersClass)
import           Pos.Util.Chrono              (NE, NewestFirst (..),
                                               OldestFirst (..))

----------------------------------------------------------------------------
-- Declarations
----------------------------------------------------------------------------

data ExplorerBListenerTag

type ExplorerBListener = Ether.TaggedTrans ExplorerBListenerTag IdentityT

-- Kind of... Empty and devoid of meaning...
runExplorerBListener :: ExplorerBListener m a -> m a
runExplorerBListener = coerce

-- Explorer implementation for usual node. Combines the operations.
instance ( MonadRealDB m
         , MonadDBRead m
         , MonadMockable m
         , WithLogger m
         )
         => MonadBListener (ExplorerBListener m) where
    onApplyBlocks     blunds = do
        epochBlocks <- onApplyEpochBlocksExplorer blunds
        pageBlocks  <- onApplyPageBlocksExplorer blunds
        pure $ SomeBatchOp [epochBlocks, pageBlocks]

    onRollbackBlocks  blunds = do
        epochBlocks <- onRollbackEpochBlocksExplorer blunds
        pageBlocks  <- onRollbackPageBlocksExplorer blunds
        pure $ SomeBatchOp [epochBlocks, pageBlocks]

-- onGeneralEpochBlocksExplorer is viable, since the only thing that needs 
-- to be defined are reversed operations (adding and removing)

----------------------------------------------------------------------------
-- Apply
----------------------------------------------------------------------------

-- For @EpochBlocks@
onApplyEpochBlocksExplorer
    :: forall ssc m .
    ( SscHelpersClass ssc
    , WithLogger m
    , MonadRealDB m
    , MonadDBRead m
    )
    => OldestFirst NE (Blund ssc) -> m SomeBatchOp
onApplyEpochBlocksExplorer blunds = do
    -- Get the epoch and the attached block @HeaderHash@es.
    let epoch    = epochBlocks ^. _1
    let blockHHs = epochBlocks ^. _2
    
    -- Get all blocks from the current epoch (so we can remove everything and
    -- add the (Epoch, [HeaderHash]) pair back to the database).
    mBlocksHHs <- DB.getEpochBlocks epoch
    let blocksHHsOrEmpty = fromMaybe mempty mBlocksHHs
    let uniqueHHs = nub $ blocksHHsOrEmpty <> blockHHs

    -- In the end make sure we place this under @SomeBatchOp@ in order to
    -- preserve atomicity.
    pure $ SomeBatchOp $ DB.PutEpochBlocks epoch uniqueHHs
  where
    epochBlocks = minEpochBlocks blocksNE

    blocksNE :: NE (Block ssc)  
    blocksNE = fst <$> getOldestFirst blunds


-- For @PageBlocks@
onApplyPageBlocksExplorer
    :: forall ssc m .
    ( SscHelpersClass ssc
    , WithLogger m
    , MonadRealDB m
    , MonadDBRead m
    )
    => OldestFirst NE (Blund ssc) -> m SomeBatchOp
onApplyPageBlocksExplorer blunds = do
    -- [(Integer, [HeaderHash])]
    let newPageBlocks = blocksPagePages blocksNE
    let pages = fst <$> newPageBlocks
    -- Get existing @HeaderHash@es from the pages so we can merge them.
    existingPageBlocks <- sequence [ getPagePageBlocks page | page <- pages]

    -- convert to Map
    let newPageBlocksMap      = M.fromList newPageBlocks
    let existingPageBlocksMap = M.fromList existingPageBlocks

    -- Merge the old and the new
    let newExistingPageBlocks = M.toList $ M.unionWith (<>) newPageBlocksMap existingPageBlocksMap

    let deletedPages = [ DB.DelPageBlocks page | page <- pages]
    let createdPages = [ DB.PutPageBlocks (pB ^. _1) (pB ^. _2) | pB <- newExistingPageBlocks]

    -- In the end make sure we place this under @SomeBatchOp@ in order to
    -- preserve atomicity.
    pure $ SomeBatchOp [deletedPages, createdPages]
  where
    blocksNE :: NE (Block ssc)  
    blocksNE = fst <$> getOldestFirst blunds

    -- Get exisiting page blocks paired with the page number. If there are no
    -- saved blocks on the page return an empty list.
    getPagePageBlocks 
        :: (MonadDBRead m) 
        => Page 
        -> m (Page, [HeaderHash])
    getPagePageBlocks page = do
        pageBlocks      <- DB.getPageBlocks page
        let mPageBlocks = fromMaybe [] pageBlocks
        pure (page, mPageBlocks)

----------------------------------------------------------------------------
-- Rollback
----------------------------------------------------------------------------

-- For @EpochBlocks@
onRollbackEpochBlocksExplorer
    :: forall ssc m .
    ( SscHelpersClass ssc
    , WithLogger m
    , MonadRealDB m
    , MonadDBRead m
    )
    => NewestFirst NE (Blund ssc) -> m SomeBatchOp
onRollbackEpochBlocksExplorer blunds = do
    let epoch    = epochBlocks ^. _1
    pure $ SomeBatchOp $ DB.DelEpochBlocks epoch
  where
    epochBlocks = minEpochBlocks blocksNE

    blocksNE :: NE (Block ssc)  
    blocksNE = fst <$> getNewestFirst blunds

onRollbackPageBlocksExplorer
    :: forall ssc m .
    ( SscHelpersClass ssc
    , WithLogger m
    , MonadRealDB m
    , MonadDBRead m
    )
    => NewestFirst NE (Blund ssc) -> m SomeBatchOp
onRollbackPageBlocksExplorer blunds = do
    let newPageBlocks = blocksPagePages blocksNE
    let pages = fst <$> newPageBlocks

    let deletedPages = [ DB.DelPageBlocks page | page <- pages]

    pure $ SomeBatchOp deletedPages
  where
    blocksNE :: NE (Block ssc)  
    blocksNE = fst <$> getNewestFirst blunds

----------------------------------------------------------------------------
-- Common
----------------------------------------------------------------------------

-- Get the least epoch, since we want to deal with a single epoch at a time!
-- IMPORTANT: This presumes: 
--    batch (blocks) <= epoch_size
-- Otherwise we will miss some blocks!
--
-- TODO: Not optimal
minEpochBlocks 
    :: forall ssc. (SscHelpersClass ssc)
    => NE (Block ssc) 
    -> (Epoch, [HeaderHash])
minEpochBlocks neBlocks = (minEpoch, minEpochBlocksHH)
  where
    -- I presume there are blocks from a single epoch as the comment in 
    -- `applyBlocksUnsafeDo` says.
    -- What about the case when the blocks are empty? Possible?
    minEpoch :: Epoch
    minEpoch = minimum $ epochBlocks blocks

    minEpochBlocksHH :: [HeaderHash]
    minEpochBlocksHH = headerHash <$> blocks

    epochBlocks :: [Block ssc] -> [Epoch]
    epochBlocks blocks' = getBlockEpoch <$> blocks'

    getBlockEpoch :: (Block ssc) -> Epoch
    getBlockEpoch block = block ^. epochIndexL

    blocks :: [Block ssc]
    blocks = NE.toList neBlocks

-- Get the (Page, [HeaderHash]) pair from the blocks
blocksPagePages 
    :: forall ssc. (SscHelpersClass ssc) 
    => NE (Block ssc) 
    -> [(Page, [HeaderHash])]
blocksPagePages neBlocks = blocksPages blockIndexBlock
  where
    -- Page number, page element
    blocksPages :: [(Page, HeaderHash)] -> [(Page, [HeaderHash])]
    blocksPages blockIndexBlock' = 
        M.toList $ M.fromListWith (++) [(k, [v]) | (k, v) <- blockIndexBlock']

    blockIndexBlock :: [(Page, HeaderHash)]
    blockIndexBlock = blockPages `zip` blocksHHs
      where 
        blocksHHs :: [HeaderHash]
        blocksHHs = headerHash <$> blocks 

        blockPages :: [Page]
        blockPages = currentPage <$> blockIndexes

        currentPage :: Int -> Page
        currentPage blockIndex = blockIndex `rem` pageSize

        pageSize :: Int
        pageSize = 10

    blockIndexes :: [Int]
    blockIndexes = getBlockIndex <$> blocks

    getBlockIndex :: (Block ssc) -> Int
    getBlockIndex block = fromIntegral $ getChainDifficulty $ block ^. difficultyL

    blocks :: [Block ssc]
    blocks = NE.toList neBlocks