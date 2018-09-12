-- | Definition of the data type representing functions with finite support and
-- operations on them.

module Chain.Abstract.FinitelySupportedFunction
  ( FinitelySupportedFunction
  , fSupport
  , fApply
  , fSum)
where

import           Data.Foldable (fold, foldMap)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Monoid (Monoid)
import           Data.Set (Set)
import           Universum hiding (fold, foldMap)

-- | Function with finite support. Note that the paper defines these as taking
--   values in a Semiring, but there's nothing intrinsic to that constraint and
--   Monoid is in the standard libraries.
class Monoid v => FinitelySupportedFunction f k v | f -> k v where
  fSupport :: f -> Set k
  -- | Apply the function. This must return `mempty` when the key is not in the support
  -- of the function.
  fApply :: f -> k -> v
  fSum :: f -> v
  fSum fn = foldMap (fApply fn) . fSupport $ fn

-- | Standard implementation of a finitely supported function as a map.
instance (Ord k, Monoid v) => FinitelySupportedFunction (Map k v) k v where
  fSupport = Map.keysSet
  fApply m k = Map.findWithDefault mempty k m
  fSum = fold . Map.elems
