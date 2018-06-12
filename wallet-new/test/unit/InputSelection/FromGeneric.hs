{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
module InputSelection.FromGeneric (
    -- * Instantiation of the generic infrastructure
    DSL
  , SafeValue(..)
  , runCoinSelT
    -- * Wrap coin selection
    -- ** Random
  , PrivacyMode(..)
  , random
    -- ** Largest-first
  , largestFirst
  ) where

import           Universum

import qualified Data.Set as Set
import qualified Data.Text.Buildable
import           Formatting (bprint, build)

import           Cardano.Wallet.Kernel.CoinSelection.Generic
import qualified Cardano.Wallet.Kernel.CoinSelection.Generic.LargestFirst as Generic
import           Cardano.Wallet.Kernel.CoinSelection.Generic.Random (PrivacyMode (..))
import qualified Cardano.Wallet.Kernel.CoinSelection.Generic.Random as Generic

import           InputSelection.TxStats
import           Util.GenHash
import qualified UTxO.DSL as DSL

{-------------------------------------------------------------------------------
  Coin selection domain
-------------------------------------------------------------------------------}

data DSL (h :: * -> *) (a :: *)

instance (DSL.Hash h a, Buildable a) => CoinSelDom (DSL h a) where
  type Input  (DSL h a) = DSL.Input  h a
  type Output (DSL h a) = DSL.Output h a
  type Value  (DSL h a) = SafeValue  h a

  outVal = Value . DSL.outVal

  valueZero = safeZero
  valueAdd  = safeAdd
  valueSub  = safeSub
  valueMult = safeMult
  valueDist = safeDist

instance CoinSelDom (DSL h a) => PickFromUtxo (DSL.Utxo h a) where
  type Dom (DSL.Utxo h a) = DSL h a

  pickRandom    u = fmap (bimap identity DSL.utxoFromMap) <$>
                      mapRandom (DSL.utxoToMap u)
  pickLargest n u = fmap (bimap identity DSL.utxoFromMap) $
                      nLargestFromMapBy DSL.outVal n (DSL.utxoToMap u)

{-------------------------------------------------------------------------------
  Auxiliary: safe wrapper around values
-------------------------------------------------------------------------------}

-- | Safe wrapper around values
newtype SafeValue (h :: * -> *) a = Value { fromSafeValue :: DSL.Value }
  deriving (Eq, Ord)

safeZero :: SafeValue h a
safeZero = Value 0

-- | TODO: check for overflow
safeAdd :: SafeValue h a -> SafeValue h a -> Maybe (SafeValue h a)
safeAdd (Value x) (Value y) = Just $ Value (x + y)

-- TODO: check for overflow
safeMult :: SafeValue h a -> Int -> Maybe (SafeValue h a)
safeMult (Value x) n = Just $ Value (x * fromIntegral n)

safeSub :: SafeValue h a -> SafeValue h a -> Maybe (SafeValue h a)
safeSub (Value x) (Value y) = do
    guard (y <= x)
    return $ Value (x - y)

safeDist :: SafeValue h a -> SafeValue h a -> SafeValue h a
safeDist (Value x) (Value y) =
    Value $ if y <= x then x - y else y - x

{-------------------------------------------------------------------------------
  Top-level coin selection
-------------------------------------------------------------------------------}

runCoinSelT :: forall utxo e h a m.
               (GenHash m, Dom utxo ~ DSL h a, CoinSelDom (DSL h a))
            => a      -- ^ Change address
            -> CoinSelT utxo e m [CoinSelResult (Dom utxo)]
            -> utxo   -- ^ Available UTxO
            -> m (Either e ((DSL.Transaction h a, TxStats), utxo))
runCoinSelT changeAddr policy utxo = do
    mSelection <- unwrapCoinSelT policy utxo
    case mSelection of
      Left err -> return (Left err)
      Right (cssWithDust, utxo') -> do
          let css = map (coinSelRemoveDust valueZero) cssWithDust
          tx <- mkTx changeAddr css
          return $ Right ((tx, deriveTxStats css), utxo')

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

deriveTxStats :: CoinSelDom (DSL h a) => [CoinSelResult (DSL h a)] -> TxStats
deriveTxStats = deriveTxStats' computeRatio
  where
    computeRatio :: SafeValue h a -> SafeValue h a -> Fixed E2
    computeRatio (Value val) (Value change) =
        fromIntegral change / fromIntegral val

{-------------------------------------------------------------------------------
  Wrap the generic coin selection algorithms
-------------------------------------------------------------------------------}

random :: (MonadRandom m, GenHash m, Dom utxo ~ DSL h a, PickFromUtxo utxo)
       => PrivacyMode
       -> a     -- ^ Change address
       -> Int   -- ^ Maximum number of inputs
       -> CoinSelPolicy utxo m (DSL.Transaction h a, TxStats)
random privacy changeAddr maxInps =
      runCoinSelT changeAddr
    . Generic.random privacy maxInps

largestFirst :: (GenHash m, Dom utxo ~ DSL h a, PickFromUtxo utxo)
             => a     -- ^ Change address
             -> Int   -- ^ Maximum number of inputs
             -> CoinSelPolicy utxo m (DSL.Transaction h a, TxStats)
largestFirst changeAddr maxInps =
      runCoinSelT changeAddr
    . Generic.largestFirst maxInps

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

instance Buildable (SafeValue h a) where
  build (Value a) = bprint build a
