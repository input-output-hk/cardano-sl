{-# LANGUAGE BangPatterns #-}

module Cardano.Wallet.Kernel.CoinSelection.Generic.Random (
    PrivacyMode(..)
  , random
  , findRandomOutput
  ) where

import           Universum

import           Cardano.Wallet.Kernel.CoinSelection.Generic
import qualified Cardano.Wallet.Kernel.CoinSelection.Generic.LargestFirst as LargestFirst

{-------------------------------------------------------------------------------
  Random input selection algorithm
-------------------------------------------------------------------------------}

data PrivacyMode = PrivacyModeOn | PrivacyModeOff

-- | Random input selection
--
-- Random input selection has the advantage that is it self correcting, in the
-- following sense: suppose that 90% of our UTxO consists of small outputs;
-- then random selection has a 90% change of choosing those small outputs.
--
-- For each output we add a change output that is between 0.5 and 2 times the
-- size of the output, making it hard to identify. This has the additional
-- benefit of introducing another self-correction: if there are frequent
-- requests for payments around certain size, the UTxO will contain lots of
-- available change outputs of around that size.
random :: forall utxo m. (MonadRandom m, PickFromUtxo utxo)
       => PrivacyMode         -- ^ Hide change addresses?
       -> Word64              -- ^ Maximum number of inputs
       -> [Output (Dom utxo)] -- ^ Outputs to include
       -> CoinSelT utxo CoinSelHardErr m [CoinSelResult (Dom utxo)]
random privacyMode initMaxNumInputs goals = do
    balance <- gets utxoBalance
    when (balance == valueZero) $ throwError (errUtxoExhausted balance)
    coinSelPerGoal selection initMaxNumInputs goals
  where
    errUtxoExhausted :: Value (Dom utxo) -> CoinSelHardErr
    errUtxoExhausted balance = CoinSelHardErrUtxoExhausted
        (pretty balance)
        (pretty $ unsafeValueSum $ map outVal goals)

    selection
        :: Word64
        -> Output (Dom utxo)
        -> CoinSelT utxo CoinSelHardErr m (CoinSelResult (Dom utxo))
    selection maxNumInputs goal = defCoinSelResult goal
        <$> inRange maxNumInputs (target privacyMode (outVal goal))

    target :: PrivacyMode -> Value (Dom utxo) -> TargetRange (Dom utxo)
    target PrivacyModeOn  val = fromMaybe (target PrivacyModeOff val)
                                          (idealRange val)
    target PrivacyModeOff val = TargetRange {
                                    targetMin = val
                                  , targetAim = val
                                  , targetMax = val
                                  }

    idealRange :: Value (Dom utxo) -> Maybe (TargetRange (Dom utxo))
    idealRange val = do
        -- Minimum value: no change at all
        let targetMin = val
        -- Ideal case: change equal to the value
        targetAim <- valueAdjust RoundUp 2.0 val
        -- Terminating condition: change twice the value
        targetMax <- valueAdjust RoundUp 3.0 val
        return TargetRange{..}

{-------------------------------------------------------------------------------
  Lower level API
-------------------------------------------------------------------------------}

-- | Target range for picking inputs
data TargetRange dom = TargetRange {
        targetMin :: Value dom
      , targetAim :: Value dom
      , targetMax :: Value dom
      }

-- | Select random inputs in the specified range
--
-- If we exceed the maximum number of inputs whilst trying to reach the minimum
-- end of the range, fallback on largest first to cover the minimum, then
-- proceed as normal with random selection to try and improve the change amount.
inRange :: (PickFromUtxo utxo, MonadRandom m)
        => Word64
        -> TargetRange (Dom utxo)
        -> CoinSelT utxo CoinSelHardErr m (SelectedUtxo (Dom utxo))
inRange maxNumInputs TargetRange{..} = do
        atLeastWithFallback maxNumInputs targetMin
    >>= improve maxNumInputs targetAim targetMax

-- | Select random inputs to cover the required minimum value.
--
-- Falls back on 'LargestFirst.atLeast' if we exceed 'maxNumInputs'
atLeastWithFallback :: forall utxo m. (PickFromUtxo utxo, MonadRandom m)
                    => Word64
                    -> Value (Dom utxo)
                    -> CoinSelT utxo CoinSelHardErr m (SelectedUtxo (Dom utxo))
atLeastWithFallback maxNumInputs targetMin =
    atLeastNoFallback    maxNumInputs targetMin `catchJustSoft` \_ ->
    LargestFirst.atLeast maxNumInputs targetMin

-- | Select random inputs to cover the required minimum value.
--
-- Fails if we exceed 'maxNumInputs'
atLeastNoFallback :: forall utxo m. (PickFromUtxo utxo, MonadRandom m)
                  => Word64
                  -> Value (Dom utxo)
                  -> CoinSelT utxo CoinSelErr m (SelectedUtxo (Dom utxo))
atLeastNoFallback maxNumInputs targetMin = go emptySelection
  where
    go :: SelectedUtxo (Dom utxo)
       -> CoinSelT utxo CoinSelErr m (SelectedUtxo (Dom utxo))
    go selected
      | sizeToWord (selectedSize selected) > maxNumInputs =
          throwError $ CoinSelErrSoft CoinSelSoftErr
      | selectedBalance selected >= targetMin =
          return selected
      | otherwise = do
          io <- mapCoinSelErr CoinSelErrHard $ findRandomOutput
          go $ select io selected

-- | Select random additional inputs with the aim of improving the change amount
--
-- This never throws an error.
improve :: forall utxo e m. (PickFromUtxo utxo, MonadRandom m)
        => Word64                  -- ^ Total maximum number of inputs
        -> Value (Dom utxo)        -- ^ Total UTxO balance to aim for
        -> Value (Dom utxo)        -- ^ Maximum total UTxO balance
        -> SelectedUtxo (Dom utxo) -- ^ UTxO selected so far
        -> CoinSelT utxo e m (SelectedUtxo (Dom utxo))
improve maxNumInputs targetAim targetMax = go
  where
    -- Preconditions
    --
    -- > 0 <= acc < targetAim
    --
    -- Invariant:
    --
    -- > acc == utxoBalance selected
    --
    -- Relies on the following self-correcting property: if the UTxO
    -- has many small entries, then we should be able to reach close
    -- to the aim value. BUT if this is the case, then the probability
    -- that when we pick a random value from the UTxO that we overshoot
    -- the upper end of the range is low. Here we terminate early if we
    -- happen to pick a value from the UTxO that overshoots the upper
    -- of the range; this is likely to happen precisely when we have
    -- a low probability of finding a value close to the aim.
    go :: SelectedUtxo (Dom utxo) -> CoinSelT utxo e m (SelectedUtxo (Dom utxo))
    go selected = do
        mIO <- tryFindRandomOutput isImprovement
        case mIO of
          Nothing        -> return selected
          Just selected' ->
            if selectedBalance selected' >= targetAim
              then return selected'
              else go selected'
     where
       -- A new value is an improvement if
       --
       -- * We don't overshoot the upper end of the range
       -- * We get closer to the aim
       -- * We don't use more than the maximum number of inputs
       --
       -- Note that the second property is a bit subtle: it is trivially
       -- true if both @acc@ and @acc + val@ are smaller than @targetAim@
       --
       -- > value | ------|------------|----------------|-------------
       -- >              acc      (acc + val)      targetAim
       --
       -- but if @acc + val@ exceeds the aim, we are comparing (absolute)
       -- distance to the aim
       --
       -- > value | ------|-----------|---------------|--------
       -- >              acc      targetAim      (acc + val)
       isImprovement :: UtxoEntry (Dom utxo) -> Maybe (SelectedUtxo (Dom utxo))
       isImprovement io = do
           guard $ and [
               selectedBalance selected' <= targetMax
             ,   valueDist targetAim (selectedBalance selected')
               < valueDist targetAim (selectedBalance selected)
             , sizeToWord (selectedSize selected') <= maxNumInputs
             ]
           return selected'
         where
           selected' = select io selected

{-------------------------------------------------------------------------------
  Auxiliary: selecting random outputs
-------------------------------------------------------------------------------}

-- | Select a random output
findRandomOutput :: (MonadRandom m, PickFromUtxo utxo)
                 => CoinSelT utxo e m (Maybe (UtxoEntry (Dom utxo)))
findRandomOutput =
    tryFindRandomOutput Just

-- | Find a random output, and return it if it satisfies the predicate
--
-- If the predicate is not satisfied, state is not changed.
tryFindRandomOutput :: forall utxo e m a. (MonadRandom m, PickFromUtxo utxo)
                    => (UtxoEntry (Dom utxo) -> Maybe a)
                    -> CoinSelT utxo e m (Maybe a)
tryFindRandomOutput p = do
    utxo <- get
    mIO  <- (>>= p') <$> pickRandom utxo
    case mIO of
      Nothing         -> return Nothing
      Just (a, utxo') -> do put utxo' ; return $ Just a
  where
    p' :: (UtxoEntry (Dom utxo), utxo) -> Maybe (a, utxo)
    p' (io, utxo) = (, utxo) <$> p io
