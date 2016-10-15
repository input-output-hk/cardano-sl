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
       , mayBlockBeUseful

       , ProcessBlocksRes (..)
       , blkProcessNewBlocks
       , blkRollback
       ) where

import           Control.Lens  (at, ix, makeClassy, preview, view, (^.))
import           Data.Default  (Default, def)
import           Data.SafeCopy (base, deriveSafeCopySimple)
import           Data.Vector   (Vector)
import           Universum

import           Pos.Crypto    (hash)
import           Pos.Genesis   (genesisLeaders)
import           Pos.Types     (Block, EpochIndex, HeaderHash, MainBlockHeader,
                                SlotLeaders, blockHeader, blockLeaders, mkGenesisBlock)

data BlockStorage = BlockStorage
    { -- | All blocks known to the node. Blocks have pointers to other
      -- blocks and can be easily traversed.
      _blkBlocks        :: !(HashMap HeaderHash Block)
    , -- | Hashes of genesis blocks in the __best chain__.
      _blkGenesisBlocks :: !(Vector HeaderHash)
    , -- | Hash of the head in the __best chain__.
      _blkHead          :: !HeaderHash
    }

makeClassy ''BlockStorage
deriveSafeCopySimple 0 'base ''BlockStorage

instance Default BlockStorage where
    def =
        BlockStorage
        { _blkBlocks = [(genesisBlock0Hash, genesisBlock0)]
        , _blkGenesisBlocks = [genesisBlock0Hash]
        , _blkHead = genesisBlock0Hash
        }
      where
        genesisBlock0 = Left (mkGenesisBlock Nothing 0 genesisLeaders)
        genesisBlock0Hash = hash $ genesisBlock0 ^. blockHeader

type Query a = forall m x. (HasBlockStorage x, MonadReader x m) => m a
type Update a = forall m x. (HasBlockStorage x, MonadState x m) => m a

-- | Get block by hash of its header.
getBlock :: HeaderHash -> Query (Maybe Block)
getBlock h = view (blkBlocks . at h)

-- | Get list of slot leaders for the given epoch. Empty list is returned
-- if no information is available.
getLeaders :: EpochIndex -> Query SlotLeaders
getLeaders (fromIntegral -> epoch) = do
    blkIdx <- preview (blkGenesisBlocks . ix epoch)
    maybe (pure mempty) (fmap leadersFromBlock . getBlock) blkIdx
  where
    leadersFromBlock (Just (Left genBlock)) = genBlock ^. blockLeaders
    leadersFromBlock _                      = mempty

-- | Check that block header is correct and claims to represent block
-- which may become part of blockchain.
mayBlockBeUseful :: MainBlockHeader -> Query Bool
mayBlockBeUseful _ = pure True

-- | Result of processNewBlocks.
data ProcessBlocksRes
    = -- | Blocks may be useful, but can't be adopted without getting
      -- more blocks. Hash of required block is returned.
      PBRmore HeaderHash
    | -- | Blocks have been adopted.
      PBRdone
    | -- | Blocks are invalid.
      PBRabort

deriveSafeCopySimple 0 'base ''ProcessBlocksRes

-- | Process new received blocks. This is interactive process.
blkProcessNewBlocks :: [Block] -> Update ProcessBlocksRes
blkProcessNewBlocks _ = pure PBRabort

-- | Rollback last `n` blocks.
blkRollback :: Int -> Update ()
blkRollback _ = pure ()
