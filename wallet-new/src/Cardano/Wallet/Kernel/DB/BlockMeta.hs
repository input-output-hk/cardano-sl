-- | Block metadata conform the wallet specification
module Cardano.Wallet.Kernel.DB.BlockMeta (
    -- * Block metadata
    BlockMeta(..)
  , AddressMeta(..)
    -- ** Lenses
  , addressMetaIsChange
  , addressMetaIsUsed
  , blockMetaAddressMeta
  , blockMetaSlotId
  ) where

import           Universum

import           Control.Lens.TH (makeLenses)
import qualified Data.Map.Strict as Map
import           Data.SafeCopy (SafeCopy (..), base, contain, deriveSafeCopy,
                     safeGet, safePut)

import qualified Pos.Core as Core

import           Cardano.Wallet.Kernel.DB.InDb

import           Data.Semigroup (Semigroup)

{-------------------------------------------------------------------------------
  Block metadata
-------------------------------------------------------------------------------}

-- | Address metadata
data AddressMeta = AddressMeta {
      -- | Whether or not an Address has been 'used'
      _addressMetaIsUsed   :: Bool
    , -- | Whether or not this is a 'change' Address
      _addressMetaIsChange :: Bool
    }

-- | Block metadata
data BlockMeta = BlockMeta {
      -- | Slot each transaction got confirmed in
      _blockMetaSlotId      :: InDb (Map Core.TxId Core.SlotId)
    , -- | Address metadata
      _blockMetaAddressMeta :: InDb (Map Core.Address AddressMeta)
    }

makeLenses ''AddressMeta
makeLenses ''BlockMeta

deriveSafeCopy 1 'base ''AddressMeta

-- TODO @uroboros/ryan [CBR 305] Implement Safecopy instances independently from legacy wallet
instance SafeCopy (InDb (Map Core.Address AddressMeta)) where
    putCopy (InDb h) = contain $ safePut h
    getCopy = contain $ InDb <$> safeGet

deriveSafeCopy 1 'base ''BlockMeta

-- | Monoid instance to update 'BlockMeta' in 'applyBlock' (see wallet spec)
instance Semigroup BlockMeta where
  a <> b = BlockMeta {
          _blockMetaSlotId = combineUsing (liftA2 Map.union) _blockMetaSlotId
        ,
          _blockMetaAddressMeta
              = combineUsing (liftA2 (Map.unionWith mergeAddrMeta)) _blockMetaAddressMeta
    }
    where
      mergeAddrMeta :: AddressMeta -> AddressMeta -> AddressMeta
      mergeAddrMeta (AddressMeta used change) (AddressMeta used' change')
          = AddressMeta (used || used') (change `xor` change')

      combineUsing :: (a -> a -> a) -> (BlockMeta -> a) -> a
      combineUsing op f = f a `op` f b

instance Monoid BlockMeta where
  mempty = BlockMeta {
           _blockMetaSlotId = InDb Map.empty
         ,
          -- NOTE: if an address does not appear in blockMetaAddressMeta, we assume
          -- that (AddressMeta isUsed isChange) = (AddressMeta False False)
          _blockMetaAddressMeta = InDb Map.empty
      }
  mappend = (<>)
