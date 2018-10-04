module Pos.Chain.Block.HasPrevBlock
       ( HasPrevBlock (..)
       ) where

import           Universum

import           Control.Lens (choosing)
import           Pos.Chain.Block.Header (BlockHeader, GenericBlockHeader,
                     HeaderHash, choosingBlockHeader, gbhPrevBlock)
import           Pos.Util.Some (Some, liftLensSome)

--------------------------------------------------------------------------------
-- HasPrevBlock
--------------------------------------------------------------------------------

-- | Lens for accessing the @HeaderHash@ of the previous block
class HasPrevBlock s where
    prevBlockL :: Lens' s HeaderHash

instance HasPrevBlock (Some HasPrevBlock) where
    prevBlockL = liftLensSome prevBlockL

instance (HasPrevBlock s, HasPrevBlock s') =>
         HasPrevBlock (Either s s') where
    prevBlockL = choosing prevBlockL prevBlockL

-- Perhaps it is not the best instance.
instance {-# OVERLAPPABLE #-} HasPrevBlock s => HasPrevBlock (s, z) where
    prevBlockL = _1 . prevBlockL

instance HasPrevBlock BlockHeader where
    prevBlockL = choosingBlockHeader prevBlockL prevBlockL

instance HasPrevBlock (GenericBlockHeader bodyProof consensus extra) where
    prevBlockL = gbhPrevBlock
