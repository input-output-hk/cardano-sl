{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TemplateHaskell       #-}

-- | Blocks maintenance happens here.

module Pos.State.Storage.Block
       (
         BlockStorage
       , HasBlockStorage (blockStorage)
       ) where

import           Control.Lens  (makeClassy)
import           Data.Default  (Default, def)
import           Data.SafeCopy (base, deriveSafeCopySimple)
import           Universum

import           Pos.Types     (Block, HeaderHash)

data BlockStorage = BlockStorage
    { -- | The best valid blockchain known to the node. We should take
      -- into account that we are dealing with tree, not list. This list
      -- is the best chain. We use builtin lists for simplicity,
      -- because we don't care about performance for now.
      _blkBlocks      :: ![Block]
    , -- | Extra blocks from alternative chains. It serves as cache basically.
      _blkExtraBlocks :: !(HashMap HeaderHash Block)
    }

makeClassy ''BlockStorage
deriveSafeCopySimple 0 'base ''BlockStorage

instance Default BlockStorage where
    def =
        BlockStorage
        { _blkBlocks = undefined
        , _blkExtraBlocks = mempty
        }

type Query a = forall m x. (HasBlockStorage x, MonadReader x m) => m a
type Update a = forall m x. (HasBlockStorage x, MonadState x m) => m a
