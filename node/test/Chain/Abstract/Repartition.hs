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
  , balanceStake
  , mkRepartition
  , mkRepartitionT
  )
where

import           Universum

import           Control.Monad.Except (MonadError, throwError)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Monoid (Sum (Sum))
import           Data.Validation (Validation (Failure), validation)

import           Chain.Abstract.FinitelySupportedFunction
                     (FinitelySupportedFunction)

-- | The stake repartition is a function that assigns stake to addresses.
--
-- To calculate the
data Repartition a =
  forall f. FinitelySupportedFunction f a (Sum Word64) => Repartition f

-- | Errors that can be returned by the smart constructors.
data RepartitionError a
  = EmptySupport
  -- ^ There is no stake assigned to any address.
  deriving Show

-- | Create a new repartition, using the given map. A `Left` value is returned
-- if the map does not constitute a valid repartition.
mkRepartition
  :: forall a . Ord a
  => [(a, Word64)]
  -> Validation [RepartitionError a] (Repartition a)
mkRepartition rs =
  Repartition rMap <$ nonEmptySupport
  where
    rMap :: Map a (Sum Word64)
    rMap = Map.fromList $ fmap (second Sum) rs

    nonEmptySupport :: Validation [RepartitionError a] ()
    nonEmptySupport =
      case filter ((0 <) . snd) rs of
        [] -> Failure [EmptySupport]
        _  -> pure ()

mkRepartitionT
  :: forall e m a . (Ord a, MonadError e m)
  => ([RepartitionError a] -> e)
  -> [(a, Word64)]
  -> m (Repartition a)
mkRepartitionT liftErr rs =
  validation (throwError . liftErr) return (mkRepartition rs)

balanceStake
  :: forall a. Ord a
  => NonEmpty a
  -> Repartition a
balanceStake addrs = Repartition $ Map.fromList (toList addrs `zip` repeat 1)
