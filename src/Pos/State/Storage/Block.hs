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

       , blkProcessBlock
       , blkRollback
       , blkSetHead
       ) where

import           Control.Lens            (at, ix, makeClassy, preview, view, (.=), (^.))
import           Data.Default            (Default, def)

import           Data.SafeCopy           (base, deriveSafeCopySimple)
import           Data.Vector             (Vector)
import           Serokell.Util           (VerificationRes)
import           Universum

import           Pos.Crypto              (hash)
import           Pos.Genesis             (genesisLeaders)
import           Pos.State.Storage.Types (AltChain, ProcessBlockRes (..))
import           Pos.Types               (Block, EpochIndex, HeaderHash, MainBlockHeader,
                                          SlotLeaders, blockHeader, blockLeaders,
                                          mkGenesisBlock)

data BlockStorage = BlockStorage
    { -- | All blocks known to the node. Blocks have pointers to other
      -- blocks and can be easily traversed.
      _blkBlocks        :: !(HashMap HeaderHash Block)
    , -- | Hashes of genesis blocks in the __best chain__.
      _blkGenesisBlocks :: !(Vector HeaderHash)
    , -- | Hash of the head in the __best chain__.
      _blkHead          :: !HeaderHash
    , -- | Alternative chains which can be merged into main chain.
      _blkAltChains     :: ![AltChain]
    }

makeClassy ''BlockStorage
deriveSafeCopySimple 0 'base ''BlockStorage

instance Default BlockStorage where
    def =
        BlockStorage
        { _blkBlocks = [(genesisBlock0Hash, genesisBlock0)]
        , _blkGenesisBlocks = [genesisBlock0Hash]
        , _blkHead = genesisBlock0Hash
        , _blkAltChains = mempty
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

deriveSafeCopySimple 0 'base ''VerificationRes
deriveSafeCopySimple 0 'base ''ProcessBlockRes

-- | Process received block.
blkProcessBlock :: Block -> Update ProcessBlockRes
blkProcessBlock _ = pure $ PBRabort mempty

-- | Set head of main blockchain to block which is guaranteed to represent valid chain.
blkSetHead :: HeaderHash -> Update ()
blkSetHead = (blkHead .=)

-- | Rollback last `n` blocks.
blkRollback :: Int -> Update ()
blkRollback _ = pure ()
