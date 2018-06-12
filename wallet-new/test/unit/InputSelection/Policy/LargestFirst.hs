module InputSelection.Policy.LargestFirst (
    largestFirst
  ) where

import           Universum

import           Control.Lens ((%=))
import           Control.Monad.Except (MonadError (..))

import           InputSelection.Policy
import           InputSelection.Policy.InputPolicyT
import           InputSelection.SortedUtxo (SortedUtxo)
import qualified InputSelection.SortedUtxo as Sorted
import qualified Util.MultiSet as MultiSet
import           UTxO.DSL (Utxo)
import qualified UTxO.DSL as DSL

{-------------------------------------------------------------------------------
  Always find the largest UTxO possible
-------------------------------------------------------------------------------}

-- | Always use largest UTxO possible
--
-- NOTE: This is a very efficient implementation. Doesn't really matter, this
-- is just for testing; we're not actually considering using such a policy.
largestFirst :: forall h a m. (RunPolicy m a, Hash h a)
             => InputSelectionPolicy SortedUtxo h a m
largestFirst utxo = \goals -> runInputPolicyT utxo $
    mconcat <$> mapM go goals
  where
    go :: Output a
       -> InputPolicyT SortedUtxo InputSelectionHardError h a m PartialTxStats
    go goal@(Output _a val) = do
        sorted   <- use ipsUtxo
        selected <- case select sorted DSL.utxoEmpty 0 of
                      Nothing -> throwError InputSelectionHardError
                      Just u  -> return u

        ipsUtxo             %= utxoRemoveInputs selected
        ipsSelectedInputs   %= DSL.utxoUnion selected
        ipsGeneratedOutputs %= (goal :)

        let selectedSum = utxoBalance selected
            change      = selectedSum - val

        unless (change == 0) $ do
          changeAddr <- genChangeAddr
          ipsGeneratedOutputs %= (Output changeAddr change :)

        return PartialTxStats {
            ptxStatsNumInputs = utxoSize selected
          , ptxStatsRatios    = MultiSet.singleton (fromIntegral change / fromIntegral val)
          }
      where
        select :: SortedUtxo h a   -- ^ Available UTxO
               -> Utxo h a         -- ^ Accumulated selected UTxO
               -> Value            -- ^ Accumulated value
               -> Maybe (Utxo h a)
        select _         acc accSum | accSum >= val = Just acc
        select available acc accSum = do
            ((i, o), available') <- Sorted.maxView available
            select available' (DSL.utxoInsert (i, o) acc) (accSum + outVal o)
