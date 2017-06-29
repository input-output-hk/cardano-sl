{-# LANGUAGE ScopedTypeVariables #-}

-- | Blockchain listener for Explorer.
-- Callbacks on application and rollback.

module Pos.Explorer.BListener
       ( runExplorerBListener 
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

import           Mockable                     (MonadMockable)
import           Pos.Block.BListener          (MonadBListener (..))
import           Pos.Block.Core               (Block)
import           Pos.Block.Types              (Blund)
import           Pos.Core                     (HeaderHash, difficultyL,
                                               epochIndexL, getChainDifficulty,
                                               headerHash)
import           Pos.DB.BatchOp               (SomeBatchOp (..))
import           Pos.DB.Class                 (MonadDBRead)
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

-- Type alias, remove duplication
type MonadBListenerT m ssc = 
    ( SscHelpersClass ssc
    , WithLogger m
    , MonadCatch m
    , MonadDBRead m
    )

-- Explorer implementation for usual node. Combines the operations.
instance ( MonadDBRead m
         , MonadMockable m
         , MonadCatch m
         , WithLogger m
         )
         => MonadBListener (ExplorerBListener m) where
    onApplyBlocks     blunds = onApplyCallGeneral blunds
    onRollbackBlocks  blunds = onRollbackCallGeneral blunds

----------------------------------------------------------------------------
-- General calls
----------------------------------------------------------------------------

onApplyCallGeneral
    :: forall ssc m . 
    MonadBListenerT m ssc
    => OldestFirst NE (Blund ssc) -> m SomeBatchOp
onApplyCallGeneral    blunds = do
    epochBlocks <- onApplyEpochBlocksExplorer blunds
    pageBlocks  <- onApplyPageBlocksExplorer blunds
    pure $ SomeBatchOp [epochBlocks, pageBlocks]


onRollbackCallGeneral
    :: forall ssc m .
    MonadBListenerT m ssc
    => NewestFirst NE (Blund ssc) -> m SomeBatchOp
onRollbackCallGeneral blunds = do
    epochBlocks <- onRollbackEpochBlocksExplorer blunds
    pageBlocks  <- onRollbackPageBlocksExplorer blunds
    pure $ SomeBatchOp [epochBlocks, pageBlocks]

----------------------------------------------------------------------------
-- Function calls
----------------------------------------------------------------------------

-- For @EpochBlocks@
onApplyEpochBlocksExplorer
    :: forall ssc m.
    MonadBListenerT m ssc
    => OldestFirst NE (Blund ssc) 
    -> m SomeBatchOp
onApplyEpochBlocksExplorer blunds = do

    -- Get existing @HeaderHash@es from the keys so we can merge them.
    existingBlocks <- getExistingBlocks $ M.keys newBlocks

    -- Merge the new and the old
    let mergedBlocksMap = M.unionWith (<>) existingBlocks newBlocks

    -- Create database operation
    let mergedBlocks = putKeysBlocks mergedBlocksMap

    -- In the end make sure we place this under @SomeBatchOp@ in order to
    -- preserve atomicity.
    pure $ SomeBatchOp mergedBlocks
  where

    newBlocks :: M.Map Epoch [HeaderHash]
    newBlocks = epochBlocksMap blocksNE

    blocksNE :: NE (Block ssc)  
    blocksNE = fst <$> getOldestFirst blunds

    -- Get exisiting key blocks paired with the key.
    getExistingBlocks
        :: (MonadDBRead m) 
        => [Epoch]
        -> m (M.Map Epoch [HeaderHash])
    getExistingBlocks keys = do
        keyBlocks <- sequence [ getExistingKeyBlocks key | key <- keys ]
        pure $ M.unions keyBlocks
      where
        -- Get exisiting key blocks paired with the key. If there are no
        -- saved blocks on the key return an empty list.
        getExistingKeyBlocks 
            :: (MonadDBRead m)
            => Epoch 
            -> m (M.Map Epoch [HeaderHash])
        getExistingKeyBlocks key = do
            keyBlocks        <- DB.getEpochBlocks key
            let mKeyBlocks = fromMaybe [] keyBlocks
            pure $ M.singleton key mKeyBlocks

    -- For each (k, [v]) pair, create a database operation.
    putKeysBlocks 
        :: M.Map Epoch [HeaderHash]
        -> [DB.ExplorerOp]
    putKeysBlocks keysBlocks = putKeyBlocks <$> M.toList keysBlocks
      where
        putKeyBlocks 
            :: (Epoch, [HeaderHash])
            -> DB.ExplorerOp
        putKeyBlocks keyBlocks = DB.PutEpochBlocks key uniqueBlocks
          where
            key           = keyBlocks ^. _1
            blocks        = keyBlocks ^. _2
            uniqueBlocks  = ordNub blocks


-- For @PageBlocks@
onApplyPageBlocksExplorer
    :: forall ssc m .
    MonadBListenerT m ssc
    => OldestFirst NE (Blund ssc) 
    -> m SomeBatchOp
onApplyPageBlocksExplorer blunds = do

    -- Get existing @HeaderHash@es from the keys so we can merge them.
    existingBlocks <- getExistingBlocks $ M.keys newBlocks

    -- Merge the new and the old
    let mergedBlocksMap = M.unionWith (<>) existingBlocks newBlocks

    -- Create database operation
    let mergedBlocks = putKeysBlocks mergedBlocksMap

    -- In the end make sure we place this under @SomeBatchOp@ in order to
    -- preserve atomicity.
    pure $ SomeBatchOp mergedBlocks
  where

    newBlocks :: M.Map Page [HeaderHash]
    newBlocks = pageBlocksMap blocksNE

    blocksNE :: NE (Block ssc)  
    blocksNE = fst <$> getOldestFirst blunds

    -- Get exisiting key blocks paired with the key.
    getExistingBlocks
        :: (MonadDBRead m) 
        => [Page]
        -> m (M.Map Page [HeaderHash])
    getExistingBlocks keys = do
        keyBlocks <- sequence [ getExistingKeyBlocks key | key <- keys ]
        pure $ M.unions keyBlocks
      where
        -- Get exisiting key blocks paired with the key. If there are no
        -- saved blocks on the key return an empty list.
        getExistingKeyBlocks 
            :: (MonadDBRead m)
            => Page 
            -> m (M.Map Page [HeaderHash])
        getExistingKeyBlocks key = do
            keyBlocks        <- DB.getPageBlocks key
            let mKeyBlocks = fromMaybe [] keyBlocks
            pure $ M.singleton key mKeyBlocks

    -- For each (k, [v]) pair, create a database operation.
    putKeysBlocks 
        :: M.Map Page [HeaderHash]
        -> [DB.ExplorerOp]
    putKeysBlocks keysBlocks = putKeyBlocks <$> M.toList keysBlocks
      where
        putKeyBlocks 
            :: (Page, [HeaderHash])
            -> DB.ExplorerOp
        putKeyBlocks keyBlocks = DB.PutPageBlocks key uniqueBlocks
          where
            key           = keyBlocks ^. _1
            blocks        = keyBlocks ^. _2
            uniqueBlocks  = ordNub blocks
    

----------------------------------------------------------------------------
-- Rollback
----------------------------------------------------------------------------

-- For @EpochBlocks@
onRollbackEpochBlocksExplorer
    :: forall ssc m .
    MonadBListenerT m ssc
    => NewestFirst NE (Blund ssc) -> m SomeBatchOp
onRollbackEpochBlocksExplorer blunds = do
    
    -- Get existing @HeaderHash@es from the keys so we can merge them.
    existingBlocks <- getExistingBlocks keys

    -- Diffrence between the new and the old
    let mergedBlocksMap = rollbackedBlocks keys newBlocks existingBlocks

    -- Create database operation
    let mergedBlocks = putKeysBlocks mergedBlocksMap

    -- In the end make sure we place this under @SomeBatchOp@ in order to
    -- preserve atomicity.
    pure $ SomeBatchOp mergedBlocks
  where

    keys :: [Epoch]
    keys = M.keys newBlocks

    newBlocks :: M.Map Epoch [HeaderHash]
    newBlocks = epochBlocksMap blocksNE

    blocksNE :: NE (Block ssc)  
    blocksNE = fst <$> getNewestFirst blunds

    -- Get exisiting key blocks paired with the key.
    getExistingBlocks
        :: (MonadDBRead m) 
        => [Epoch]
        -> m (M.Map Epoch [HeaderHash])
    getExistingBlocks keys' = do
        keyBlocks <- sequence [ getExistingKeyBlocks key | key <- keys' ]
        pure $ M.unions keyBlocks
      where
        -- Get exisiting key blocks paired with the key. If there are no
        -- saved blocks on the key return an empty list.
        getExistingKeyBlocks 
            :: (MonadDBRead m)
            => Epoch 
            -> m (M.Map Epoch [HeaderHash])
        getExistingKeyBlocks key = do
            keyBlocks        <- DB.getEpochBlocks key
            let mKeyBlocks = fromMaybe [] keyBlocks
            pure $ M.singleton key mKeyBlocks

    -- For each (k, [v]) pair, create a database operation.
    putKeysBlocks 
        :: M.Map Epoch [HeaderHash]
        -> [DB.ExplorerOp]
    putKeysBlocks keysBlocks = putKeyBlocks <$> M.toList keysBlocks
      where
        putKeyBlocks 
            :: (Epoch, [HeaderHash])
            -> DB.ExplorerOp
        putKeyBlocks keyBlocks = DB.PutEpochBlocks key uniqueBlocks
          where
            key           = keyBlocks ^. _1
            blocks        = keyBlocks ^. _2
            uniqueBlocks  = ordNub blocks

    -- The result is the map that contains diffed blocks from the keys.
    rollbackedBlocks
        :: [Epoch]
        -> M.Map Epoch [HeaderHash]
        -> M.Map Epoch [HeaderHash]
        -> M.Map Epoch [HeaderHash]
    rollbackedBlocks keys' oldMap' newMap' = M.unions rolledbackKeyMaps
      where
        rolledbackKeyMaps = [ rollbackedKey key oldMap' newMap' | key <- keys' ]

        -- From a single key, retrieve blocks and return their difference
        rollbackedKey
            :: Epoch
            -> M.Map Epoch [HeaderHash]
            -> M.Map Epoch [HeaderHash]
            -> M.Map Epoch [HeaderHash]
        rollbackedKey key oldMap newMap = elementsExist
          where
            newBlocks' :: [HeaderHash]
            newBlocks' = newMap ^. at key . non []

            existingBlocks :: [HeaderHash]
            existingBlocks = oldMap ^. at key . non []

            elementsExist :: M.Map Epoch [HeaderHash]
            elementsExist = M.singleton key finalBlocks
              where
                -- Rollback the new blocks, remove them from the collection
                finalBlocks :: [HeaderHash]
                finalBlocks = existingBlocks \\ newBlocks'


onRollbackPageBlocksExplorer
    :: forall ssc m .
    MonadBListenerT m ssc
    => NewestFirst NE (Blund ssc) -> m SomeBatchOp
onRollbackPageBlocksExplorer blunds = do
    
    -- Get existing @HeaderHash@es from the keys so we can merge them.
    existingBlocks <- getExistingBlocks keys

    -- Diffrence between the new and the old
    let mergedBlocksMap = rollbackedBlocks keys newBlocks existingBlocks

    -- Create database operation
    let mergedBlocks = putKeysBlocks mergedBlocksMap

    -- In the end make sure we place this under @SomeBatchOp@ in order to
    -- preserve atomicity.
    pure $ SomeBatchOp mergedBlocks
  where
    
    keys :: [Page]
    keys = M.keys newBlocks

    newBlocks :: M.Map Page [HeaderHash]
    newBlocks = pageBlocksMap blocksNE

    blocksNE :: NE (Block ssc)  
    blocksNE = fst <$> getNewestFirst blunds

    -- Get exisiting key blocks paired with the key.
    getExistingBlocks
        :: (MonadDBRead m) 
        => [Page]
        -> m (M.Map Page [HeaderHash])
    getExistingBlocks keys' = do
        keyBlocks <- sequence [ getExistingKeyBlocks key | key <- keys' ]
        pure $ M.unions keyBlocks
      where
        -- Get exisiting key blocks paired with the key. If there are no
        -- saved blocks on the key return an empty list.
        getExistingKeyBlocks 
            :: (MonadDBRead m)
            => Page 
            -> m (M.Map Page [HeaderHash])
        getExistingKeyBlocks key = do
            keyBlocks        <- DB.getPageBlocks key
            let mKeyBlocks = fromMaybe [] keyBlocks
            pure $ M.singleton key mKeyBlocks

    -- For each (k, [v]) pair, create a database operation.
    putKeysBlocks 
        :: M.Map Page [HeaderHash]
        -> [DB.ExplorerOp]
    putKeysBlocks keysBlocks = putKeyBlocks <$> M.toList keysBlocks
      where
        putKeyBlocks 
            :: (Page, [HeaderHash])
            -> DB.ExplorerOp
        putKeyBlocks keyBlocks = DB.PutPageBlocks key uniqueBlocks
          where
            key           = keyBlocks ^. _1
            blocks        = keyBlocks ^. _2
            uniqueBlocks  = ordNub blocks

    -- The result is the map that contains diffed blocks from the keys.
    rollbackedBlocks
        :: [Page]
        -> M.Map Page [HeaderHash]
        -> M.Map Page [HeaderHash]
        -> M.Map Page [HeaderHash]
    rollbackedBlocks keys' oldMap' newMap' = M.unions rolledbackKeyMaps
      where
        rolledbackKeyMaps = [ rollbackedKey key oldMap' newMap' | key <- keys' ]

        -- From a single key, retrieve blocks and return their difference
        rollbackedKey
            :: Page
            -> M.Map Page [HeaderHash]
            -> M.Map Page [HeaderHash]
            -> M.Map Page [HeaderHash]
        rollbackedKey key oldMap newMap = elementsExist
          where
            newBlocks' :: [HeaderHash]
            newBlocks' = newMap ^. at key . non []

            existingBlocks :: [HeaderHash]
            existingBlocks = oldMap ^. at key . non []

            elementsExist :: M.Map Page [HeaderHash]
            elementsExist = M.singleton key finalBlocks
              where
                -- Rollback the new blocks, remove them from the collection
                finalBlocks :: [HeaderHash]
                finalBlocks = existingBlocks \\ newBlocks'

----------------------------------------------------------------------------
-- Common
----------------------------------------------------------------------------

epochBlocksMap 
    :: forall ssc. (SscHelpersClass ssc)
    => NE (Block ssc) 
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

    getBlockEpoch :: (Block ssc) -> Epoch
    getBlockEpoch block = block ^. epochIndexL

    blocks :: [Block ssc]
    blocks = NE.toList neBlocks


pageBlocksMap 
    :: forall ssc. (SscHelpersClass ssc) 
    => NE (Block ssc) 
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
        getCurrentPage blockIndex = (blockIndex `div` pageSize) + 1

        pageSize :: Int
        pageSize = 10

    blockIndexes :: [Int]
    blockIndexes = getBlockIndex <$> blocks

    getBlockIndex :: (Block ssc) -> Int
    getBlockIndex block = fromIntegral $ getChainDifficulty $ block ^. difficultyL

    blocks :: [Block ssc]
    blocks = NE.toList neBlocks