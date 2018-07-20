module Cardano.Wallet.Kernel.Util (
    -- * Lists
    at
    -- * Maps and sets
  , disjoint
  , withoutKeys
  , restrictKeys
    -- * Dealing with OldestFirst/NewestFirst
  , liftOldestFirst
  , liftNewestFirst
    -- * Probabilities
  , Probability
  , toss
    -- * Operations on UTxO
  , utxoBalance
  , utxoRestrictToInputs
  , paymentAmount
    -- * General-utility functions
  , getCurrentTimestamp
  ) where

import           Universum

import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Pos.Core.Chrono
import qualified Test.QuickCheck as QC

import qualified Pos.Core as Core
import qualified Pos.Txp as Core

{-------------------------------------------------------------------------------
  Lists
-------------------------------------------------------------------------------}

-- | Safe version of (!!)
at :: [a] -> Int -> Maybe a
at []     _ = Nothing
at (x:_)  0 = Just x
at (_:xs) i = at xs (i - 1)

{-------------------------------------------------------------------------------
  Maps and sets
-------------------------------------------------------------------------------}

-- | Check that two sets are disjoint
--
-- This is available out of the box from containters >= 0.5.11
disjoint :: Ord a => Set a -> Set a -> Bool
disjoint a b = Set.null (a `Set.intersection` b)

withoutKeys :: Ord k => Map k a -> Set k -> Map k a
m `withoutKeys` s = m `Map.difference` Map.fromSet (const ()) s

restrictKeys :: Ord k => Map k a -> Set k -> Map k a
m `restrictKeys` s = m `Map.intersection` Map.fromSet (const ()) s

{-------------------------------------------------------------------------------
  Dealing with OldestFirst/NewestFirst
-------------------------------------------------------------------------------}

liftOldestFirst :: (f a -> f a) -> OldestFirst f a -> OldestFirst f a
liftOldestFirst f = OldestFirst . f . getOldestFirst

liftNewestFirst :: (f a -> f a) -> NewestFirst f a -> NewestFirst f a
liftNewestFirst f = NewestFirst . f . getNewestFirst

{-------------------------------------------------------------------------------
  Probabilities
-------------------------------------------------------------------------------}

-- | Probability (value between 0 and 1)
type Probability = Double

-- | Weighted coin toss
--
-- @toss p@ throws a p-weighted coin and returns whether it came up heads.
-- @toss 0@ will always return @False@, @toss 1@ will always return @True@.
toss :: Probability -> QC.Gen Bool
toss 0 = return False
toss 1 = return True
toss p = (< p) <$> QC.choose (0, 1)

{-------------------------------------------------------------------------------
  Operations on UTxO
-------------------------------------------------------------------------------}

-- | Computes the balance for this 'Utxo'. We use 'unsafeAddCoin' as
-- as long as the 'maxCoinVal' stays within the 'Word64' 'maxBound', the
-- circulating supply of coins is finite and as such we should never have
-- to sum an 'Utxo' which would exceed the bounds.
-- If it does, this is clearly a bug and we throw an 'ErrorCall' exception
-- (crf. 'unsafeAddCoin' implementation).
utxoBalance :: Core.Utxo -> Core.Coin
utxoBalance = foldl' updateFn (Core.mkCoin 0) . Map.elems
    where
        updateFn :: Core.Coin -> Core.TxOutAux -> Core.Coin
        updateFn acc txOutAux =
            acc `Core.unsafeAddCoin` (toCoin txOutAux)

-- | Restricts the 'Utxo' to only the selected set of inputs.
utxoRestrictToInputs :: Set Core.TxIn -> Core.Utxo -> Core.Utxo
utxoRestrictToInputs inps utxo = utxo `restrictKeys` inps

-- | Calculates the amount of a requested payment.
paymentAmount :: NonEmpty Core.TxOut -> Core.Coin
paymentAmount = Core.unsafeIntegerToCoin
              . Core.sumCoins
              . map Core.txOutValue
              . toList

-- | Gets the underlying value (as a 'Coin') from a 'TxOutAux'.
toCoin :: Core.TxOutAux -> Core.Coin
toCoin = Core.txOutValue . Core.toaOut

{-------------------------------------------------------------------------------
  General-utility functions
-------------------------------------------------------------------------------}

-- (NOTE: we are abandoning the 'Mockable time' strategy of the Cardano code base)
getCurrentTimestamp :: IO Core.Timestamp
getCurrentTimestamp = Core.Timestamp . round . (* 1000000) <$> getPOSIXTime

