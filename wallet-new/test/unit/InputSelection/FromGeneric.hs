{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
module InputSelection.FromGeneric (
    -- * Instantiation of the generic infrastructure
    DSL
  , SafeValue(..)
  , Size(..)
  , runCoinSelT
    -- * Wrap coin selection
    -- ** Random
  , PrivacyMode(..)
  , random
    -- ** Largest-first
  , largestFirst
  ) where

import           Universum

import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import           Formatting (bprint, build)
import qualified Formatting.Buildable
import qualified Prelude

import           Cardano.Wallet.Kernel.CoinSelection.Generic
import           Cardano.Wallet.Kernel.CoinSelection.Generic.Grouped
import qualified Cardano.Wallet.Kernel.CoinSelection.Generic.LargestFirst as Generic
import           Cardano.Wallet.Kernel.CoinSelection.Generic.Random
                     (PrivacyMode (..))
import qualified Cardano.Wallet.Kernel.CoinSelection.Generic.Random as Generic

import           InputSelection.TxStats
import           Util.GenHash
import qualified UTxO.DSL as DSL

{-------------------------------------------------------------------------------
  Coin selection domain
-------------------------------------------------------------------------------}

data DSL (h :: * -> *) (a :: *)

instance IsValue (SafeValue h a) where
  valueZero   = safeZero
  valueAdd    = safeAdd
  valueSub    = safeSub
  valueDist   = safeDist
  valueRatio  = safeRatio
  valueAdjust = safeAdjust

instance (DSL.Hash h a, Buildable a) => CoinSelDom (DSL h a) where
  type    Input     (DSL h a) = DSL.Input  h a
  type    Output    (DSL h a) = DSL.Output h a
  type    Value     (DSL h a) = SafeValue  h a
  type    UtxoEntry (DSL h a) = (DSL.Input h a, DSL.Output h a)
  newtype Size      (DSL h a) = Size Word64

  outVal    = Value . DSL.outVal
  outSubFee = \(Fee v) o -> outSetVal o <$> valueSub (outVal o) v
     where
       outSetVal o (Value v) = o {DSL.outVal = v}

instance (DSL.Hash h a, Buildable a, Ord a) => HasAddress (DSL h a) where
  type Address (DSL h a) = a

  outAddr = DSL.outAddr

instance (DSL.Hash h a, Buildable a) => StandardDom (DSL h a)
instance (DSL.Hash h a, Buildable a) => StandardUtxo (DSL.Utxo h a)

instance (DSL.Hash h a, Buildable a) => PickFromUtxo (DSL.Utxo h a) where
  type Dom (DSL.Utxo h a) = DSL h a
  -- Use default implementations

instance (DSL.Hash h a, Buildable a, Ord a) => CanGroup (DSL.Utxo h a) where
  -- Use default implementations

{-------------------------------------------------------------------------------
  Auxiliary: safe wrapper around values
-------------------------------------------------------------------------------}

-- | Safe wrapper around values
newtype SafeValue (h :: * -> *) a = Value { fromSafeValue :: DSL.Value }
  deriving (Eq, Ord)

-- | Don't print the constructor, just the value
instance Show (SafeValue h a) where
  show = Prelude.show . fromSafeValue

instance Read (SafeValue h a) where
  readsPrec p = map (bimap Value identity) . Prelude.readsPrec p

safeZero :: SafeValue h a
safeZero = Value 0

-- TODO: check for overflow
safeAdd :: SafeValue h a -> SafeValue h a -> Maybe (SafeValue h a)
safeAdd (Value x) (Value y) = Just $ Value (x + y)

safeSub :: SafeValue h a -> SafeValue h a -> Maybe (SafeValue h a)
safeSub (Value x) (Value y) = do
    guard (y <= x)
    return $ Value (x - y)

safeDist :: SafeValue h a -> SafeValue h a -> SafeValue h a
safeDist (Value x) (Value y) =
    Value $ if y <= x then x - y else y - x

safeRatio :: SafeValue h a -> SafeValue h a -> Double
safeRatio (Value x) (Value y) =
    fromIntegral x / fromIntegral y

-- TODO: check for underflow/overflow
safeAdjust :: Rounding -> Double -> SafeValue h a -> Maybe (SafeValue h a)
safeAdjust RoundUp   d (Value x) = Just $ Value $ ceiling (d * fromIntegral x)
safeAdjust RoundDown d (Value x) = Just $ Value $ floor   (d * fromIntegral x)

{-------------------------------------------------------------------------------
  Top-level coin selection
-------------------------------------------------------------------------------}

runCoinSelT :: forall utxo e h a m.
               (GenHash m, Dom utxo ~ DSL h a, CoinSelDom (DSL h a))
            => a      -- ^ Change address
            -> CoinSelT utxo e m [CoinSelResult (Dom utxo)]
            -> utxo   -- ^ Available UTxO
            -> m (Either e (DSL.Transaction h a, TxStats, utxo))
runCoinSelT changeAddr policy utxo = do
    mSelection <- unwrapCoinSelT policy utxo
    case mSelection of
      Left err -> return (Left err)
      Right (cssWithDust, utxo') -> do
          let css = map (coinSelRemoveDust valueZero) cssWithDust
          tx <- mkTx changeAddr css
          return $ Right (tx, deriveTxStats css, utxo')

mkTx :: forall h a m. (GenHash m, CoinSelDom (DSL h a))
     => a -> [CoinSelResult (DSL h a)] -> m (DSL.Transaction h a)
mkTx changeAddr css = do
    h <- genHash
    return DSL.Transaction {
          DSL.trFresh = 0
        , DSL.trIns   = Set.unions $ map coinSelInputSet css
        , DSL.trOuts  = concatMap mkOutputs css
        , DSL.trFee   = 0 -- TODO: deal with fees
        , DSL.trHash  = h
        , DSL.trExtra = []
        }
  where
    mkOutputs :: CoinSelResult (DSL h a) -> [DSL.Output h a]
    mkOutputs cs = coinSelOutput cs : map mkChangeOutput (coinSelChange cs)

    mkChangeOutput :: Value (DSL h a) -> DSL.Output h a
    mkChangeOutput (Value change) = DSL.Output changeAddr change

{-------------------------------------------------------------------------------
  Wrap the generic coin selection algorithms
-------------------------------------------------------------------------------}

random :: (MonadRandom m, GenHash m, Dom utxo ~ DSL h a, PickFromUtxo utxo)
       => PrivacyMode
       -> a      -- ^ Change address
       -> Word64 -- ^ Maximum number of inputs
       -> CoinSelPolicy utxo m (DSL.Transaction h a, TxStats, utxo)
random privacy changeAddr maxInps =
      runCoinSelT changeAddr
    . Generic.random privacy maxInps
    . NE.toList

largestFirst :: (GenHash m, Dom utxo ~ DSL h a, PickFromUtxo utxo)
             => a      -- ^ Change address
             -> Word64 -- ^ Maximum number of inputs
             -> CoinSelPolicy utxo m (DSL.Transaction h a, TxStats, utxo)
largestFirst changeAddr maxInps =
      runCoinSelT changeAddr
    . Generic.largestFirst maxInps
    . NE.toList

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

instance Buildable (SafeValue h a) where
  build (Value a) = bprint build a
