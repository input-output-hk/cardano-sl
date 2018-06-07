module InputSelection.Policy.LargestFirst (
    largestFirst
  ) where

import           Universum

import           Control.Lens ((%=))
import           Control.Monad.Except (MonadError (..))
import qualified Data.Set as Set

import           InputSelection.Policy
import           InputSelection.Policy.InputPolicyT
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
             => InputSelectionPolicy Utxo h a m
largestFirst utxo = \goals -> runInputPolicyT utxo $
    mconcat <$> mapM go goals
  where
    go :: Output a -> InputPolicyT Utxo h a m PartialTxStats
    go goal@(Output _a val) = do
        sorted   <- sortBy sortKey . DSL.utxoToList <$> use ipsUtxo
        selected <- case select sorted DSL.utxoEmpty 0 of
                      Nothing -> throwError InputSelectionFailure
                      Just u  -> return u

        ipsUtxo             %= DSL.utxoRemoveInputs (DSL.utxoDomain selected)
        ipsSelectedInputs   %= Set.union (DSL.utxoDomain selected)
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
        select :: [(Input h a, Output a)] -- ^ Sorted available UTxO
               -> Utxo h a                -- ^ Selected UTxO
               -> Value                   -- ^ Accumulated value
               -> Maybe (Utxo h a)
        select _                   acc accSum | accSum >= val = Just acc
        select []                  _   _      = Nothing
        select ((i, o):available') acc accSum =
            select available' (DSL.utxoInsert (i, o) acc) (accSum + outVal o)

    -- Sort by output value, descending
    sortKey :: (Input h a, Output a) -> (Input h a, Output a) -> Ordering
    sortKey = flip (comparing (outVal . snd))
