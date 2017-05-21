{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Block maintenance in wallet state

module Pos.Wallet.State.Storage.Block
       ( BlockStorage (..)
       , HasBlockStorage (blockStorage)

       , Block'

       , getBlock
       , getBestChain

       , blkSetHead
       ) where

import           Universum

import           Control.Lens              (at, makeClassy, (.=))
import           Control.Monad.Loops       (unfoldrM)
import           Control.Monad.Trans.Maybe (MaybeT (..))
import           Data.Default              (Default, def)
import qualified Data.HashMap.Strict       as HM
import           Data.SafeCopy             (base, deriveSafeCopySimple)

import           Pos.Block.Core            (Block)
import           Pos.Core                  (HeaderHash, prevBlockL)
import           Pos.Crypto                (unsafeHash)
import           Pos.SafeCopy              ()
import           Pos.Ssc.GodTossing        (SscGodTossing)

type Block' = Block SscGodTossing

data BlockStorage = BlockStorage
    { -- | All blocks known to the node. Blocks have pointers to other
      -- blocks and can be easily traversed.
      _blkBlocks    :: !(HashMap HeaderHash Block')
    , -- | Hash of the head in the best chain.
      _blkHead      :: !HeaderHash
    , -- | Hash of bottom block (of depth `k + 1`, or, if the whole
      -- chain is shorter than `k + 1`, the first block in the chain)
      _blkBottom    :: !HeaderHash
    , -- | Alternative chains which can be merged into main chain.
      _blkNEBlockss :: ![NonEmpty Block']
    }

makeClassy ''BlockStorage
deriveSafeCopySimple 0 'base ''BlockStorage

instance Default BlockStorage where
    def = BlockStorage HM.empty (unsafeHash (0 :: Word8)) (unsafeHash (1 :: Word8)) []

type Query a = forall m x. (HasBlockStorage x, MonadReader x m) => m a
type Update a = forall m x. (HasBlockStorage x, MonadState x m) => m a

-- | Get block by hash of its header.
getBlock :: HeaderHash -> Query (Maybe Block')
getBlock h = view (blkBlocks . at h)

getBestChain :: Query [Block']
getBestChain = do
    headHash <- view blkHead
    botHash <- view blkBottom
    flip unfoldrM headHash $ \h -> runMaybeT $
        if h == botHash
        then mzero
        else do blk <- MaybeT $ getBlock h
                return $ (blk, blk ^. prevBlockL)

-- | Set head of main blockchain to block which is guaranteed to
-- represent valid chain and be stored in blkBlocks.
blkSetHead :: HeaderHash -> Update ()
blkSetHead headHash = blkHead .= headHash
