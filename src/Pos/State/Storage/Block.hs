{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE ViewPatterns          #-}

-- | Blocks maintenance happens here.

module Pos.State.Storage.Block
       (
         BlockStorage
       , HasBlockStorage (blockStorage)

       , getBlock
       , getLeaders
       ) where

import           Control.Lens  (makeClassy, views, (^.))
import           Data.Default  (Default, def)
import           Data.SafeCopy (base, deriveSafeCopySimple)
import           Data.Vector   (Vector, (!?))
import           Universum

import           Pos.Genesis   (genesisLeaders)
import           Pos.Types     (Block, EpochIndex, HeaderHash, SlotLeaders, blockLeaders,
                                mkGenesisBlock)

data BlockStorage = BlockStorage
    { -- | The best valid blockchain known to the node. We should take
      -- into account that we are dealing with tree, not list. This
      -- list is the best chain. We use builtin lists for simplicity,
      -- because we don't care about performance for now. Head of list
      -- is the most recent block.
      _blkBlocks         :: ![Block]
    , -- | Extra blocks from alternative chains. It serves as cache basically.
      _blkExtraBlocks    :: !(HashMap HeaderHash Block)
    , -- | Indices of genesis blocks.
      _blkGenesisIndices :: !(Vector Word)
    }

makeClassy ''BlockStorage
deriveSafeCopySimple 0 'base ''BlockStorage

instance Default BlockStorage where
    def =
        BlockStorage
        { _blkBlocks = [Left (mkGenesisBlock Nothing 0 genesisLeaders)]
        , _blkExtraBlocks = mempty
        , _blkGenesisIndices = [0]
        }

type Query a = forall m x. (HasBlockStorage x, MonadReader x m) => m a
type Update a = forall m x. (HasBlockStorage x, MonadState x m) => m a

-- | Get i-th block from the best know chain. 0-th block is genesis
-- block for 0-th epoch.
getBlock :: Word -> Query (Maybe Block)
getBlock (fromIntegral -> i) =
    views blkBlocks (\b -> b `atMay` (length b - i - 1))

-- | Get list of slot leaders for the given epoch. Empty list is returned
-- if no information is available.
getLeaders :: EpochIndex -> Query SlotLeaders
getLeaders (fromIntegral -> epoch) = do
    blkIdx <- views blkGenesisIndices (!? epoch)
    maybe (pure mempty) (fmap leadersFromBlock . getBlock) blkIdx
  where
    leadersFromBlock (Just (Left genBlock)) = genBlock ^. blockLeaders
    leadersFromBlock _                      = mempty
