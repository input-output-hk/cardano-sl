module InputSelection.Policy.Random (
    PrivacyMode(..)
  , random
  ) where

import           Universum

import           Control.Lens ((%=), (.=))
import           Control.Monad.Except (MonadError (..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Test.QuickCheck hiding (Fixed)

import           InputSelection.Policy
import           InputSelection.Policy.InputPolicyT
import qualified Util.MultiSet as MultiSet
import           UTxO.DSL (Utxo)
import qualified UTxO.DSL as DSL

{-------------------------------------------------------------------------------
  Random
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
random :: forall h a m. (RunPolicy m a, LiftQuickCheck m, Hash h a)
       => PrivacyMode -> InputSelectionPolicy Utxo h a m
random privacyMode utxo = \goals -> runInputPolicyT utxo $
    mconcat <$> mapM go goals
  where
    go :: Output a -> InputPolicyT Utxo h a m PartialTxStats
    go goal@(Output _a val) = do
        -- First attempt to find a change output in the ideal range.
        -- Failing that, try to at least cover the value.
        --
        -- TODO: We should take deposit/payment ratio into account and
        -- change number of change outputs accordingly
        selected <- case privacyMode of
          PrivacyModeOff -> randomInRange fallback
          PrivacyModeOn  -> randomInRange ideal `catchError` \_err ->
                            randomInRange fallback
        ipsSelectedInputs   %= utxoUnion selected
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
        fallback, ideal :: TargetRange
        fallback = AtLeast val
        ideal    = InRange {
                       targetMin = val + (val `div` 2)
                     , targetAim = val + val
                     , targetMax = val + (val * 2)
                     }

-- | Target range for picking inputs
data TargetRange =
    -- | Cover at least the specified value, with no upper bound
    AtLeast {
        targetMin :: Value
      }

    -- | Find inputs in the specified range, aiming for the ideal value
  | InRange {
        targetMin :: Value
      , targetAim :: Value
      , targetMax :: Value
      }

-- | Random input selection: core algorithm
--
-- Select random inputs until we reach a value in the given bounds.
-- Returns the selected outputs.
randomInRange :: forall h a m. (Hash h a, LiftQuickCheck m)
              => TargetRange -> InputPolicyT Utxo h a m (Utxo h a)
randomInRange AtLeast{..} = go 0 DSL.utxoEmpty
  where
    -- Invariant:
    --
    -- > acc == utxoBalance selected
    go :: Value -> Utxo h a -> InputPolicyT Utxo h a m (Utxo h a)
    go acc selected
      | acc >= targetMin = return selected
      | otherwise        = do io@(_, out) <- findRandomOutput
                              go (acc + outVal out) (DSL.utxoInsert io selected)
randomInRange InRange{..} = go 0 DSL.utxoEmpty
  where
    -- Preconditions
    --
    -- > 0 <= acc < tAim
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
    go :: Value -> Utxo h a -> InputPolicyT Utxo h a m (Utxo h a)
    go acc selected = do
        mIO <- tryFindRandomOutput isImprovement
        case mIO of
          Nothing
            | acc  >= targetMin -> return selected
            | otherwise         -> throwError InputSelectionFailure
          Just (i, o)
            | acc' >= targetAim -> return selected'
            | otherwise         -> go acc' selected'
            where
              acc'      = acc + outVal o
              selected' = DSL.utxoInsert (i, o) selected
     where
       -- A new value is an improvement if
       --
       -- * We don't overshoot the upper end of the range
       -- * We get closer to the aim.
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
       isImprovement :: (Input h a, Output a) -> Bool
       isImprovement (_, Output _ val) =
              (acc + val) <= targetMax
           && distance targetAim (acc + val) < distance targetAim acc

       distance :: Value -> Value -> Value
       distance a b | a < b     = b - a
                    | otherwise = a - b

-- | Select a random output
findRandomOutput :: LiftQuickCheck m
                 => InputPolicyT Utxo h a m (Input h a, Output a)
findRandomOutput = do
    mIO <- tryFindRandomOutput (const True)
    case mIO of
      Just io -> return io
      Nothing -> throwError InputSelectionFailure

-- | Find a random output, and return it if it satisfies the predicate
--
-- If the predicate is not satisfied, state is not changed.
tryFindRandomOutput :: LiftQuickCheck m
                    => ((Input h a, Output a) -> Bool)
                    -> InputPolicyT Utxo h a m (Maybe (Input h a, Output a))
tryFindRandomOutput p = do
    utxo <- DSL.utxoToMap <$> use ipsUtxo
    mIO  <- liftQuickCheck $ randomElement utxo
    case mIO of
      Nothing       -> return Nothing
      Just (io, utxo')
        | p io      -> do ipsUtxo .= DSL.utxoFromMap utxo' ; return $ Just io
        | otherwise -> return Nothing

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

-- | Pick a random element from a map
--
-- Returns 'Nothing' if the map is empty
randomElement :: forall k a. Map k a -> Gen (Maybe ((k, a), Map k a))
randomElement m
  | Map.null m = return Nothing
  | otherwise  = (Just . withIx) <$> choose (0, Map.size m - 1)
  where
    withIx :: Int -> ((k, a), Map k a)
    withIx ix = (Map.elemAt ix m, Map.deleteAt ix m)
