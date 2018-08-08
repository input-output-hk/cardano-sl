{-# LANGUAGE RankNTypes #-}

-- | Block metadata conform the wallet specification
module Cardano.Wallet.Kernel.DB.BlockMeta (
    -- * Block metadata
    BlockMeta(..)
  , AddressMeta(..)
  , addressMeta
  , emptyBlockMeta
    -- * Local block metadata
  , LocalBlockMeta(..)
  , emptyLocalBlockMeta
  , appendBlockMeta
  , appendLocalBlockMeta
    -- ** Lenses
  , addressMetaIsChange
  , addressMetaIsUsed
  , blockMetaAddressMeta
  , blockMetaSlotId
  ) where

import           Universum

import           Control.Lens (at, non)
import           Control.Lens.TH (makeLenses, makeWrapped)
import qualified Data.Map.Strict as Map
import           Data.SafeCopy (SafeCopy (..), base, contain, deriveSafeCopy,
                     safeGet, safePut)
import           Formatting (bprint, build, (%))
import qualified Formatting.Buildable
import           Serokell.Util (mapJson)

import qualified Pos.Core as Core
import qualified Pos.Core.Txp as Txp

import           Cardano.Wallet.Kernel.DB.InDb

import           Data.Semigroup (Semigroup)

{-------------------------------------------------------------------------------
  Address metadata
-------------------------------------------------------------------------------}

-- | Address metadata
data AddressMeta = AddressMeta {
      -- | Whether or not an Address has been 'used'
      _addressMetaIsUsed   :: Bool
      -- | Whether or not this is a 'change' Address
    , _addressMetaIsChange :: Bool
    } deriving Eq

instance Semigroup AddressMeta where
  a <> b = mergeAddrMeta a b
    where
      mergeAddrMeta :: AddressMeta -> AddressMeta -> AddressMeta
      mergeAddrMeta (AddressMeta used change) (AddressMeta used' change')
          = AddressMeta (used || used') (change `xor` change')

instance Monoid AddressMeta where
  mempty  = AddressMeta False False
  mappend = (<>)

makeLenses ''AddressMeta
deriveSafeCopy 1 'base ''AddressMeta

{-------------------------------------------------------------------------------
  Block metadata
-------------------------------------------------------------------------------}

-- | Block metadata
data BlockMeta = BlockMeta {
      -- | Slot each transaction got confirmed in
      _blockMetaSlotId      :: InDb (Map Txp.TxId Core.SlotId)
      -- | Address metadata
    , _blockMetaAddressMeta :: InDb (Map Core.Address AddressMeta)
    }

makeLenses ''BlockMeta

-- TODO @uroboros/ryan [CBR 305] Implement Safecopy instances independently from legacy wallet
instance SafeCopy (InDb (Map Core.Address AddressMeta)) where
    putCopy (InDb h) = contain $ safePut h
    getCopy = contain $ InDb <$> safeGet

deriveSafeCopy 1 'base ''BlockMeta
-- | Address metadata for the specified address
--
-- When the block metadata does not contain any information about this address,
-- we assume 'mempty'.
addressMeta :: Core.Address -> Lens' BlockMeta AddressMeta
addressMeta addr = blockMetaAddressMeta . fromDb . at addr . non mempty

emptyBlockMeta :: BlockMeta
emptyBlockMeta = BlockMeta {
      _blockMetaSlotId      = InDb Map.empty
    , _blockMetaAddressMeta = InDb Map.empty
    }

{-------------------------------------------------------------------------------
  Local block metadata
-------------------------------------------------------------------------------}

-- | Local block metadata
--
-- Local block metadata is block metadata derived from a single block, or
-- possibly a few blocks, without access to the entire chain. The underlying
-- 'BlockMeta' type is the same; 'LocalBlockMeta' serves merely as a marker that
-- this data is potentially incomplete.
newtype LocalBlockMeta = LocalBlockMeta { localBlockMeta :: BlockMeta }

makeWrapped ''LocalBlockMeta

deriveSafeCopy 1 'base ''LocalBlockMeta

-- | Apply local block metadata
--
-- In the typical case, we have 'BlockMeta' for the chain so far, then derive
-- local blockmeta for the new block that just arrived and want to combine this
-- with the existing 'BlockMeta'.
appendBlockMeta :: BlockMeta -> LocalBlockMeta -> BlockMeta
appendBlockMeta cur (LocalBlockMeta new) = BlockMeta {
        _blockMetaSlotId      = combineUsing (liftA2 Map.union)
                                  _blockMetaSlotId
      , _blockMetaAddressMeta = combineUsing (liftA2 (Map.unionWith (<>)))
                                  _blockMetaAddressMeta
      }
  where
    combineUsing :: (a -> a -> a) -> (BlockMeta -> a) -> a
    combineUsing op f = f cur `op` f new

-- | Apply local block metadata to local block metadata
--
-- During wallet restoration we may only have local block metadata available for
-- the most recent checkpoint. In this case, we have no choice but to apply
-- the new local block metadata to the running local block metadata.
--
-- See also 'applyBlockMeta'.
appendLocalBlockMeta :: LocalBlockMeta -> LocalBlockMeta -> LocalBlockMeta
appendLocalBlockMeta (LocalBlockMeta cur) = LocalBlockMeta . appendBlockMeta cur

-- | Empty local block metadata
--
-- This corresponds to the block metadata of blocks that do not contain
-- any information that is relevant to the wallet.
emptyLocalBlockMeta :: LocalBlockMeta
emptyLocalBlockMeta = LocalBlockMeta emptyBlockMeta

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

instance Buildable AddressMeta where
    build AddressMeta{..} = bprint
        ( "AddressMeta"
        % "{ isUsed:   " % build
        % ", isChange: " % build
        % "}"
        )
        _addressMetaIsUsed
        _addressMetaIsChange

instance Buildable BlockMeta where
    build BlockMeta{..} = bprint
        ( "BlockMeta"
        % "{ slotId:      " % mapJson
        % ", addressMeta: " % mapJson
        % "}"
        )
        (_fromDb _blockMetaSlotId)
        (_fromDb _blockMetaAddressMeta)
