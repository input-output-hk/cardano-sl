{-# LANGUAGE BangPatterns #-}

module Cardano.Wallet.Kernel.CoinSelection.Generic.LargestFirst (
    largestFirst
  , atLeast
  ) where

import           Universum

import           Cardano.Wallet.Kernel.CoinSelection.Generic

{-------------------------------------------------------------------------------
  Largest-first input selection algorithm
-------------------------------------------------------------------------------}

-- | Largest-first input selection algorithm
--
-- NOTE: Not for production use! This is merely a wrapper around 'atLeast'
-- so that we can test the largest-first input selection in isolation.
-- However, the lower level API 'atLeast' /is/ used as a fallback in the
-- random input selection algorithm.
largestFirst :: (Monad m, PickFromUtxo utxo)
             => Word64              -- ^ Maximum number of inputs
             -> [Output (Dom utxo)] -- ^ Outputs to include
             -> CoinSelT utxo CoinSelHardErr m [CoinSelResult (Dom utxo)]
largestFirst = coinSelPerGoal $ \maxNumInputs goal ->
    defCoinSelResult goal <$>
      atLeast maxNumInputs (outVal goal)

{-------------------------------------------------------------------------------
  Lower-level API
-------------------------------------------------------------------------------}

-- | Select coins to cover at least the specified value
--
-- When we fail in the random selection policy because we exceeded the maximum
-- number of inputs @n@, we fallback on the 'largestFirstFallback'. We select
-- the @n@ largest inputs from the UTxO in a single linear pass, then walk over
-- these from large to small to try and cover the value we need to cover.
-- If this fails, we have no further fallbacks and this payment request is
-- not satisfiable.
--
-- If it succeeds, we can then use this as the basis for another call to
-- the random input selection to try and construct a more useful change output
-- (provided we haven't used up all available inputs yet).
atLeast :: forall utxo m. (Monad m, PickFromUtxo utxo)
        => Word64
        -> Value (Dom utxo)
        -> CoinSelT utxo CoinSelHardErr m (SelectedUtxo (Dom utxo))
atLeast maxNumInputs targetMin = do
    utxo <- get
    case go emptySelection utxo (pickLargest maxNumInputs utxo) of
      Nothing -> do
          -- If we failed to cover 'targetMin' it might be because we
          -- depleted the Utxo or simply because our 'maxNumInputs' was
          -- to stringent and in normal conditions we @would have@ covered
          -- targetMin. To diversify the two errors, if
          -- 'utxoBalance utxo >= targetMin' it means this is a max input
          -- failure, otherwise we have genuinely exhausted the utxo.
          let balance = utxoBalance utxo
          if balance < targetMin
             then throwError $ CoinSelHardErrUtxoExhausted (pretty balance) (pretty targetMin)
             else throwError $ CoinSelHardErrMaxInputsReached (pretty maxNumInputs)
      Just (selected, remainingUtxo) -> do
        put remainingUtxo
        return selected
  where
    go :: SelectedUtxo (Dom utxo)
       -> utxo
       -> [(UtxoEntry (Dom utxo), utxo)]
       -> Maybe (SelectedUtxo (Dom utxo), utxo)
    go !acc remainingUtxo sorted
      | selectedBalance acc >= targetMin = Just (acc, remainingUtxo)
      | otherwise = case sorted of
                      [] -> Nothing
                      (io, remainingUtxo'):sorted' ->
                        go (select io acc) remainingUtxo' sorted'
