module InputSelection.Policy.Random (
    PrivacyMode(..)
  , random
  ) where

import           Universum

import           Control.Lens ((%=), (.=))
import           Control.Monad.Except (MonadError (..))
import qualified Data.Map.Strict as Map
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
       => Int -> PrivacyMode -> InputSelectionPolicy Utxo h a m
random maxNumInputs privacyMode utxo = \goals -> runInputPolicyT utxo $
    mconcat <$> mapM goGoal goals
  where
    -- TODO: Thread (remaining) maxNumInputs through
    goGoal :: Output a
           -> InputPolicyT Utxo InputSelectionHardError h a m PartialTxStats
    goGoal goal@(Output _a val) = do
        selected <- randomInRange maxNumInputs (target privacyMode val)
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

    target :: PrivacyMode -> Value -> TargetRange
    target PrivacyModeOn val = TargetRange {
          targetMin = 1 * val -- no change
        , targetAim = 2 * val -- change equal to the value (ideal)
        , targetMax = 3 * val -- change twice the value (terminating condition)
        }
    target PrivacyModeOff val = TargetRange {
          targetMin = val
        , targetAim = val
        , targetMax = val
        }

{-------------------------------------------------------------------------------
  Core of the random input selection algorithm
-------------------------------------------------------------------------------}

-- | Target range for picking inputs
data TargetRange = TargetRange {
        targetMin :: Value
      , targetAim :: Value
      , targetMax :: Value
      }

-- | Select random inputs in the specified range
--
-- If we exceed the maximum number of inputs whilst trying to reach the minimum
-- end of the range, fallback on largest first to cover the minimum, then
-- proceed as normal with random selection to try and improve the change amount.
randomInRange :: (LiftQuickCheck m, Hash h a)
              => Int
              -> TargetRange
              -> InputPolicyT Utxo InputSelectionHardError h a m (Utxo h a)
randomInRange maxNumInputs TargetRange{..} = do
    required <- randomRequired maxNumInputs targetMin `catchSoftError` \_ ->
                largestFirst   maxNumInputs targetMin
    randomImprove maxNumInputs targetAim targetMax required

-- | Select random inputs to cover the required minimum value.
randomRequired :: forall h a m. (Hash h a, LiftQuickCheck m)
               => Int
               -> Value
               -> InputPolicyT Utxo InputSelectionError h a m (Utxo h a)
randomRequired maxNumInputs targetMin = go 0 DSL.utxoEmpty
  where
    -- Invariant:
    --
    -- > acc == utxoBalance selected
    go :: Value
       -> Utxo h a
       -> InputPolicyT Utxo InputSelectionError h a m (Utxo h a)
    go acc selected
      | utxoSize selected > maxNumInputs =
          throwError $ Right InputSelectionSoftError
      | acc >= targetMin =
          return selected
      | otherwise = do
          (i, o) <- mapInputPolicyErrors Left $ findRandomOutput
          go (acc + outVal o) (DSL.utxoInsert (i, o) selected)

-- | Select random additional inputs with the aim of improving the change amount
--
-- This never throws an error.
randomImprove :: forall e h a m. (Hash h a, LiftQuickCheck m)
              => Int       -- ^ Total maximum number of inputs
              -> Value     -- ^ Total UTxO balance to aim for
              -> Value     -- ^ Maximum total UTxO balance
              -> Utxo h a  -- ^ UTxO selected so far
              -> InputPolicyT Utxo e h a m (Utxo h a)
randomImprove maxNumInputs targetAim targetMax = \utxo ->
    go (utxoBalance utxo) utxo
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
    go :: Value -> Utxo h a -> InputPolicyT Utxo e h a m (Utxo h a)
    go acc selected = do
        mIO <- tryFindRandomOutput isImprovement
        case mIO of
          Nothing -> return selected
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
       isImprovement :: (Input h a, Output a) -> Bool
       isImprovement (_, Output _ val) =
              (acc + val) <= targetMax
           && distance targetAim (acc + val) < distance targetAim acc
           && utxoSize selected < maxNumInputs

       distance :: Value -> Value -> Value
       distance a b | a < b     = b - a
                    | otherwise = a - b

{-------------------------------------------------------------------------------
  Auxiliary: selecting random outputs
-------------------------------------------------------------------------------}

-- | Select a random output
findRandomOutput :: LiftQuickCheck m
                 => InputPolicyT Utxo InputSelectionHardError h a m (Input h a, Output a)
findRandomOutput = do
    mIO <- tryFindRandomOutput (const True)
    case mIO of
      Just io -> return io
      Nothing -> throwError InputSelectionHardError

-- | Find a random output, and return it if it satisfies the predicate
--
-- If the predicate is not satisfied, state is not changed.
tryFindRandomOutput :: LiftQuickCheck m
                    => ((Input h a, Output a) -> Bool)
                    -> InputPolicyT Utxo e h a m (Maybe (Input h a, Output a))
tryFindRandomOutput p = do
    utxo <- DSL.utxoToMap <$> use ipsUtxo
    mIO  <- liftQuickCheck $ randomElement utxo
    case mIO of
      Nothing       -> return Nothing
      Just (io, utxo')
        | p io      -> do ipsUtxo .= DSL.utxoFromMap utxo' ; return $ Just io
        | otherwise -> return Nothing

{-------------------------------------------------------------------------------
  Largest-first fallback
-------------------------------------------------------------------------------}

-- | Largest first fallback
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
largestFirst :: forall h a m. (Monad m, Hash h a)
             => Int
             -> Value
             -> InputPolicyT Utxo InputSelectionHardError h a m (Utxo h a)
largestFirst maxNumInputs targetMin = do
    sorted <- nLargestUtxo <$> use ipsUtxo
    case go utxoEmpty 0 sorted of
      Nothing       -> throwError InputSelectionHardError
      Just selected -> do
        ipsUtxo %= DSL.utxoRemoveInputs (DSL.utxoDomain selected)
        return selected
  where
    nLargestUtxo :: Utxo h a -> [(Input h a, Output a)]
    nLargestUtxo = nLargestBy (outVal . snd) maxNumInputs . DSL.utxoToList

    go :: Utxo h a -> Value -> [(Input h a, Output a)] -> Maybe (Utxo h a)
    go acc accBal sorted
      | accBal >= targetMin = Just acc
      | otherwise = case sorted of
                      []             -> Nothing
                      (i, o):sorted' -> go (DSL.utxoInsert (i, o) acc)
                                           (accBal + outVal o)
                                           sorted'

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

-- | Return the @n@ largest elements of the list, in no particular order.
--
-- @O(n)@
nLargestBy :: forall a b. Ord b => (a -> b) -> Int -> [a] -> [a]
nLargestBy f n = \xs ->
    let (firstN, rest) = splitAt n xs
        acc            = Map.fromListWith (++) $ map (\a -> (f a, [a])) firstN
    in go acc rest
  where
    -- We cache the minimum element in the accumulator, since looking this up
    -- is an @O(log n)@ operation.
    --
    -- Invariants:
    --
    -- * Map must contain exactly @n@ elements
    -- * No list in the codomain of the map can be empty
    --
    -- NOTE: Using a PSQ here doesn't really gain us very much. Sure, we can
    -- lookup the minimum element in @O(1)@ time, but /replacing/ the minimum
    -- element still requires @O(log n)@ time. Thus, if we cache the minimum
    -- value we have the same complexity, and avoid an additional depenedency.
    go :: Map b [a] -> [a] -> [a]
    go acc = go' acc (fst (Map.findMin acc))

    -- Inherits invariants from @go@
    -- Precondition: @accMin == fst (Map.findMin acc)@
    go' :: Map b [a] -> b -> [a] -> [a]
    go' acc _ []    = concat $ Map.elems acc
    go' acc accMin (a:as)
       | b > accMin = go (replaceMin accMin b a acc) as
       | otherwise  = go' acc accMin as
       where
         b :: b
         b = f a

    -- Replace the minimum entry in the map
    --
    -- Precondition: @accMin@ should be the minimum key of the map.
    replaceMin :: b -> b -> a -> Map b [a] -> Map b [a]
    replaceMin accMin b a = Map.insertWith (++) b [a] . Map.alter dropOne accMin

    -- Remove one entry from the map
    --
    -- All of the entries in these lists have the same "size" (@b@),
    -- so we just drop the first.
    dropOne :: Maybe [a] -> Maybe [a]
    dropOne Nothing       = error "nLargest': precondition violation"
    dropOne (Just [])     = error "nLargest': invariant violation"
    dropOne (Just [_])    = Nothing
    dropOne (Just (_:as)) = Just as
