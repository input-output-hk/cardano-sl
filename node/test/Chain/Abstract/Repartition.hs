{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Stake repartition.
--
-- The stake repartition function assigns maps addresses to a portion of the
-- total stake. As such, a stake repartition function @r@ satisfies:
--
-- - for all address a in its domain @0 <= r a@
-- - @fSum r = 1@
--
-- This module defined the 'Repartition' data type, and smart constructors that
-- check for these invariants.
--
module Chain.Abstract.Repartition
  ( Repartition
  , mkEmptyRepartition
  , mkRepartition
  )
where

import           Universum

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Monoid (Sum (Sum))
import           Data.Validation (Validation (Failure))

import           Chain.Abstract.FinitelySupportedFunction
                     (FinitelySupportedFunction, fSum)

-- | The stake repartition is a function that assigns a portion of the total
-- stake to addresses.
data Repartition a =
  forall f. FinitelySupportedFunction f a (Sum Int) => Repartition f

data RepartitionError a
  = NegativeValues (NonEmpty (a, Int))
  -- ^ Pairs with negative values.
  | SumNotOne Int
  -- ^ The sum of all repartition images does not add up to one (they add to
  -- the given value instead).
  deriving Show

-- | Create a new repartition, using the given map. A `Left` value is returned
-- if the map does not constitute a valid repartition.
mkRepartition
  :: forall a . Ord a
  => [(a, Int)]
  -> Validation [RepartitionError a] (Repartition a)
mkRepartition rs =
  pure (Repartition rMap)
  <* allNatural
  <* addUpToOne
  where
    rMap :: Map a (Sum Int)
    rMap = Map.fromList $ fmap (second Sum) rs

    allNatural :: Validation [RepartitionError a] ()
    allNatural =
      case filter ((0 <) . snd) rs of
        []   -> pure ()
        x:xs -> Failure . pure .  NegativeValues $ x:|xs

    addUpToOne :: Validation [RepartitionError a] ()
    addUpToOne = unless (1 == imgSum) (Failure . pure . SumNotOne $ imgSum)
      where
        imgSum = sum $ fmap snd rs

-- | Create an empty repartition. Note that this function always succeed since
-- an empty repartition (i.e. one whose finite support is the empty set)
-- trivially satisfies all the conditions for a repartition.
mkEmptyRepartition :: Ord a => Repartition a
mkEmptyRepartition = Repartition $ Map.empty
