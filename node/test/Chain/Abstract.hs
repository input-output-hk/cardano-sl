{-# LANGUAGE KindSignatures #-}

-- | An abstract blockchain. This sits between the pure UTxO based DSL and a
--   full-fat implementation of Cardano.

module Chain.Abstract where

import qualified Data.Foldable (fold, foldMap)
import           Data.Hashable (hash)
import qualified Data.Map.Strict as Map
import           Data.Monoid (Sum (..))
import qualified Data.Set as Set
import           Pos.Core.Chrono
import           Universum
import qualified UTxO.DSL as DSL

-- | In the abstract DSL, we identify transactions with integers.
newtype Addr = Addr Int
  deriving (Buildable, Eq, Ord, Show)

-- | Block hash
newtype BlockHash = BlockHash Int deriving (Eq)

-- | Hash of the genesis block
genesisBlockHash :: BlockHash
genesisBlockHash = BlockHash 0

-- | Invalid block hash
invalidBlockHash :: BlockHash
invalidBlockHash = BlockHash $ Data.Hashable.hash @String "invalid"

-- | Function with finite support. Note that the paper defines these as taking
--   values in a Semiring, but there's nothing intrinsic to that constraint and
--   Monoid is in the standard libraries.
class Monoid v => FinitelySupportedFunction f k v | f -> k v where
  fSupport :: f -> Set.Set k
  -- | Apply the function. This must return `mempty` when the key is not in the support
  -- of the function.
  fApply :: f -> k -> v
  fSum :: f -> v
  fSum fn = Data.Foldable.foldMap (fApply fn) . fSupport $ fn

-- | Standard implementation of a finitely supported function as a map.
instance (Ord k, Monoid v) => FinitelySupportedFunction (Map.Map k v) k v where
  fSupport = Map.keysSet
  fApply m k = Map.findWithDefault mempty k m
  fSum = Data.Foldable.fold . Map.elems

data StakeDistribution a =
  forall f. FinitelySupportedFunction f a (Sum Int) => StakeDistribution f

data Repartition a =
  forall f. FinitelySupportedFunction f a (Sum Int) => Repartition f

data Delegation (h :: * -> *) a = Delegation
  { delegator :: a
  , delegatee :: a
  } deriving (Eq)

newtype SlotId = SlotId Int deriving (Eq)

nextSlot :: SlotId -> SlotId
nextSlot (SlotId i) = SlotId $ i + 1

data Output (h :: * -> *) a = Output
  { outAddr        :: a
  , outVal         :: DSL.Value
    -- | Repartitioning of the stake associated with this transaction's inputs.
  , outRepartition :: Repartition a
  }

-- TODO(md): Have to finish implementing this one
-- instance (Show a) => Show (Output h a) where
--     show o = "Output: outAddr = " ++ outAddr o ++ ", outVal = " ++ outVal o ++ ", outRepartition = /scrambled/"

-- TODO(md): This is just a work-around for not being able to define an Eq
-- instance for Repartition a
instance Eq a => Eq (Output h a) where
    (Output {outAddr = a1, outVal = v1, outRepartition = _})
        == (Output {outAddr = a2, outVal = v2, outRepartition = _}) = a1 == a2 && v1 == v2

-- | Extract the DSL output from the abstract one.
outDSL :: Output h a -> DSL.Output h a
outDSL o = DSL.Output
  { DSL.outAddr = outAddr o
  , DSL.outVal = outVal o
  }

data Transaction h a = Transaction
  { trFresh   :: DSL.Value
  -- ^ The money that is created by this transaction. This money
  -- implicitly comes from the treasury.
  , trIns     :: NE (DSL.Input h a)
  -- ^ The set of input transactions that feed this transaction.
  , trOuts    :: NE (Output h a)
  -- ^ The list of outputs for this transaction.
  , trFee     :: DSL.Value
  -- ^ The fee charged to this transaction.
  , trHash    :: Int
  -- ^ The hash of this transaction. Must be unique in the entire chain.
  , trExtra   :: [Text]
  -- ^ Free-form comments, used for debugging
  , trWitness :: NE a
  -- ^ Transaction witnesses. There should be one witness per transaction input.
  } deriving (Eq) -- TODO(md): Add the Show class here

-- | The abstract transaction has the same hash as the underlying DSL transaction.
hash :: DSL.Hash h a
     => Transaction h a
     -> h (DSL.Transaction h a)
hash t = DSL.hash $ trDSL t

-- | Extract the DSL transaction from the abstract one.
trDSL :: DSL.Hash h a => Transaction h a -> DSL.Transaction h a
trDSL t = DSL.Transaction
  { DSL.trFresh = trFresh t
  , DSL.trIns = Set.fromList . toList $ trIns t
  , DSL.trOuts = outDSL <$> (toList $ trOuts t)
  , DSL.trFee = trFee t
  , DSL.trHash = trHash t
  , DSL.trExtra = trExtra t
  }

data Block h a = Block
  { -- | Previous block hash
    blockPred         :: BlockHash
    -- | Slot occupied by this block
  , blockSlot         :: SlotId
    -- | The address issuing this block.
  , blockIssuer       :: a
  , blockTransactions :: OldestFirst [] (Transaction h a)
  , blockDlg          :: [Delegation h a]
  } deriving (Eq)

type Chain h a = OldestFirst [] (Block h a)
