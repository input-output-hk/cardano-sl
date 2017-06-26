{-# LANGUAGE ScopedTypeVariables #-}

-- | Blockchain listener for Explorer.
-- Callbacks on application and rollback.

module Pos.Explorer.BListener
       ( runExplorerBListener -- (+) BListener instance
       ) where

import           Control.Monad.Trans.Identity (IdentityT (..))
import           Data.Coerce                  (coerce)
import qualified Ether

import           Pos.Block.Types              (Blund)
import           Pos.DB.BatchOp               (SomeBatchOp)
import           Pos.Ssc.Class.Helpers        (SscHelpersClass)
import           Pos.Util.Chrono              (NE, NewestFirst (..),
                                               OldestFirst (..))
import           Universum

import           Data.List                    (nub)
import qualified Data.List.NonEmpty           as NE
import qualified Data.Map                     as M
import           Mockable                     (MonadMockable)
import           System.Wlog                  (WithLogger)

import           Pos.Block.BListener          (MonadBListener (..))
import           Pos.Block.Core               (Block)
import           Pos.Core                     (HeaderHash, difficultyL,
                                               epochIndexL, getChainDifficulty,
                                               getEpochIndex, headerHash)
import           Pos.DB.BatchOp               (SomeBatchOp (..))
import           Pos.DB.Class                 (MonadDBRead, MonadRealDB)
import qualified Pos.Explorer.DB              as DB


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
    let newPageBlocks = blocksPagePages blocksNE
    let pages = fst <$> newPageBlocks
    -- Get existing @HeaderHash@es from the pages so we can merge them.
    mPageBlocks <- sequence [ DB.getPageBlocks page | page <- pages]
    
    let pageBlocks = fromMaybe [] <$> mPageBlocks
    let exisitingPageBlocks = pages `zip` pageBlocks

    -- convert to Map
    let newPageBlocksMap      = M.fromList newPageBlocks
    let existingPageBlocksMap = M.fromList exisitingPageBlocks

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
    -> (Integer, [HeaderHash])
minEpochBlocks neBlocks = (minEpoch, minEpochBlocksHH)
  where
    -- I presume there are blocks from a single epoch as the comment in 
    -- `applyBlocksUnsafeDo` says.
    -- What about the case when the blocks are empty? Possible?
    minEpoch :: Integer
    minEpoch = minimum $ epochBlocks blocks

    minEpochBlocksHH :: [HeaderHash]
    minEpochBlocksHH = headerHash <$> blocks

    epochBlocks :: [Block ssc] -> [Integer]
    epochBlocks blocks' = getBlockEpoch <$> blocks'

    getBlockEpoch :: (Block ssc) -> Integer
    getBlockEpoch block = toInteger $ getEpochIndex $ block ^. epochIndexL

    blocks :: [Block ssc]
    blocks = NE.toList neBlocks

-- Get the (Page, [HeaderHash]) pair from the blocks
blocksPagePages 
    :: forall ssc. (SscHelpersClass ssc) 
    => NE (Block ssc) 
    -> [(Integer, [HeaderHash])]
blocksPagePages neBlocks = blocksPages blockIndexBlock
  where
    -- Page number, page element
    blocksPages :: [(Integer, HeaderHash)] -> [(Integer, [HeaderHash])]
    blocksPages blockIndexBlock' = 
        M.toList $ M.fromListWith (++) [(k, [v]) | (k, v) <- blockIndexBlock']

    blockIndexBlock :: [(Integer, HeaderHash)]
    blockIndexBlock = blockPages `zip` blocksHHs
      where 
        blocksHHs :: [HeaderHash]
        blocksHHs = headerHash <$> blocks 

        blockPages :: [Integer]
        blockPages = currentPage <$> blockIndexes

        currentPage :: Integer -> Integer
        currentPage blockIndex = blockIndex `rem` pageSize

        pageSize :: Integer
        pageSize = 10

    blockIndexes :: [Integer]
    blockIndexes = getBlockIndex <$> blocks

    getBlockIndex :: (Block ssc) -> Integer
    getBlockIndex block = toInteger $ getChainDifficulty $ block ^. difficultyL

    blocks :: [Block ssc]
    blocks = NE.toList neBlocks