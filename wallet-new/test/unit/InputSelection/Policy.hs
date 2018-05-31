{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module InputSelection.Policy (
    -- * Infrastructure
    LiftQuickCheck(..)
  , RunPolicy(..)
  , InputSelectionPolicy
  , PrivacyMode(..)
    -- * Transaction statistics
  , TxStats(..)
    -- * Specific policies
  , largestFirst
  , random
  ) where

import           Universum

import           Control.Lens ((%=), (.=))
import           Control.Lens.TH (makeLenses)
import           Control.Monad.Except (MonadError (..))
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Test.QuickCheck

import           Util.Histogram (BinSize (..), Histogram)
import qualified Util.Histogram as Histogram
import           Util.MultiSet (MultiSet)
import qualified Util.MultiSet as MultiSet
import           UTxO.DSL

{-------------------------------------------------------------------------------
  Auxiliary: lift QuickCheck computations
-------------------------------------------------------------------------------}

-- | Monads in which we can run QuickCheck generators
class Monad m => LiftQuickCheck m where
   -- | Run a QuickCheck computation
  liftQuickCheck :: Gen x -> m x

-- | TODO: We probably don't want this instance (or abstract in a different
-- way over "can generate random numbers")
instance LiftQuickCheck IO where
  liftQuickCheck = generate

instance LiftQuickCheck m => LiftQuickCheck (StateT s m) where
  liftQuickCheck = lift . liftQuickCheck

{-------------------------------------------------------------------------------
  Transaction statistics
-------------------------------------------------------------------------------}

-- | Transaction statistics
--
-- Transaction statistics are used for policy evaluation. For "real" input
-- selection policies we don't necessarily need to return this information,
-- although it may be beneficial to do so even there -- it may be useful
-- to monitor these statistics and learn something about the wallet as it
-- operates in reality.
data TxStats = TxStats {
      -- | Number of inputs
      --
      -- This is a histogram because although a single transaction only has
      -- a single value for its number of inputs, recording this as a histogram
      -- allows us to combine the statistics of many transactions.
      txStatsNumInputs :: !Histogram

      -- | Change/payment ratios
    , txStatsRatios    :: !(MultiSet Double)
    }

instance Monoid TxStats where
  mempty = TxStats {
        txStatsNumInputs = Histogram.empty
      , txStatsRatios    = MultiSet.empty
      }
  mappend a b = TxStats {
        txStatsNumInputs = mappendUsing Histogram.add  txStatsNumInputs
      , txStatsRatios    = mappendUsing MultiSet.union txStatsRatios
      }
    where
      mappendUsing :: (a -> a -> a) -> (TxStats -> a) -> a
      mappendUsing op f = f a `op` f b

-- | Partial transaciton statistics
--
-- Partial transactions statistics are useful when constructing a transaciton
-- piece by piece.
data PartialTxStats = PartialTxStats {
      -- | Number of inputs
      --
      -- Unlike for 'TxStats', this is not a histogram. Suppose we have two
      -- 'PartialTxStats' with 'ptxStatsNumInputs' equal to @n@ and @m@.
      -- Then the final histogram should have a single bin at @n + m@ with
      -- count 1. This is rather different from having two transactions with
      -- @n@ inputs and @m@ outputs; this would result in a histogram with
      -- /two/ bins at @n@ and @m@ both with count 1, or, if @n == m@, a
      -- single bin at @n@ with count 2.
      ptxStatsNumInputs :: !Int

      -- | Change/payment ratios
    , ptxStatsRatios    :: !(MultiSet Double)
    }

instance Monoid PartialTxStats where
  mempty = PartialTxStats {
        ptxStatsNumInputs = 0
      , ptxStatsRatios    = MultiSet.empty
      }
  mappend a b = PartialTxStats {
        ptxStatsNumInputs = mappendUsing (+)            ptxStatsNumInputs
      , ptxStatsRatios    = mappendUsing MultiSet.union ptxStatsRatios
      }
    where
      mappendUsing :: (a -> a -> a) -> (PartialTxStats -> a) -> a
      mappendUsing op f = f a `op` f b

-- | Construct transaciton statistics from partial statistics
fromPartialTxStats :: PartialTxStats -> TxStats
fromPartialTxStats PartialTxStats{..} = TxStats{
      txStatsNumInputs = Histogram.singleton (BinSize 1) ptxStatsNumInputs 1
    , txStatsRatios    = ptxStatsRatios
    }

{-------------------------------------------------------------------------------
  Policy
-------------------------------------------------------------------------------}

-- | Monads in which we can run input selection policies
class Monad m => RunPolicy m a | m -> a where
  -- | Generate change address
  genChangeAddr :: m a

  -- | Generate fresh hash
  genFreshHash :: m Int

type InputSelectionPolicy h a m =
      Utxo h a
   -> [Output a]
   -> m (Either InputSelectionFailure (Transaction h a, TxStats))

data InputSelectionFailure = InputSelectionFailure

{-------------------------------------------------------------------------------
  Input selection combinator
-------------------------------------------------------------------------------}

data InputPolicyState h a = InputPolicyState {
      -- | Available entries in the UTxO
      _ipsUtxo             :: Utxo h a

      -- | Selected inputs
    , _ipsSelectedInputs   :: Set (Input h a)

      -- | Generated outputs
    , _ipsGeneratedOutputs :: [Output a]
    }

initInputPolicyState :: Utxo h a -> InputPolicyState h a
initInputPolicyState utxo = InputPolicyState {
      _ipsUtxo             = utxo
    , _ipsSelectedInputs   = Set.empty
    , _ipsGeneratedOutputs = []
    }

makeLenses ''InputPolicyState

newtype InputPolicyT h a m x = InputPolicyT {
      unInputPolicyT :: StateT (InputPolicyState h a) (ExceptT InputSelectionFailure m) x
    }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadState (InputPolicyState h a)
           , MonadError InputSelectionFailure
           )

{-
inputPolicyT :: (   InputPolicyState h a
                 -> m (Either InputSelectionFailure (x, InputPolicyState h a)))
             -> InputPolicyT h a m x
inputPolicyT f = InputPolicyT (StateT (\st -> ExceptT (f st)))
-}

instance MonadTrans (InputPolicyT h a) where
  lift = InputPolicyT . lift . lift

instance LiftQuickCheck m => LiftQuickCheck (InputPolicyT h a m) where
  liftQuickCheck = lift . liftQuickCheck

instance RunPolicy m a => RunPolicy (InputPolicyT h a m) a where
  genChangeAddr = lift genChangeAddr
  genFreshHash  = lift genFreshHash

runInputPolicyT :: RunPolicy m a
                => Utxo h a
                -> InputPolicyT h a m PartialTxStats
                -> m (Either InputSelectionFailure (Transaction h a, TxStats))
runInputPolicyT utxo policy = do
     mx <- runExceptT (runStateT (unInputPolicyT policy) initSt)
     case mx of
       Left err ->
         return $ Left err
       Right (ptxStats, finalSt) -> do
         h <- genFreshHash
         return $ Right (
             Transaction {
                 trFresh = 0
               , trIns   = finalSt ^. ipsSelectedInputs
               , trOuts  = finalSt ^. ipsGeneratedOutputs
               , trFee   = 0 -- TODO: deal with fees
               , trHash  = h
               , trExtra = []
               }
           , fromPartialTxStats ptxStats
           )
  where
    initSt = initInputPolicyState utxo

{-------------------------------------------------------------------------------
  Always find the largest UTxO possible
-------------------------------------------------------------------------------}

-- | Always use largest UTxO possible
--
-- NOTE: This is a very efficient implementation. Doesn't really matter, this
-- is just for testing; we're not actually considering using such a policy.
largestFirst :: forall h a m. (RunPolicy m a, Hash h a)
             => InputSelectionPolicy h a m
largestFirst utxo = \goals -> runInputPolicyT utxo $
    mconcat <$> mapM go goals
  where
    go :: Output a -> InputPolicyT h a m PartialTxStats
    go goal@(Output _a val) = do
        sorted   <- sortBy sortKey . utxoToList <$> use ipsUtxo
        selected <- case select sorted utxoEmpty 0 of
                      Nothing -> throwError InputSelectionFailure
                      Just u  -> return u

        ipsUtxo             %= utxoRemoveInputs (utxoDomain selected)
        ipsSelectedInputs   %= Set.union (utxoDomain selected)
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
            select available' (utxoInsert (i, o) acc) (accSum + outVal o)

    -- Sort by output value, descending
    sortKey :: (Input h a, Output a) -> (Input h a, Output a) -> Ordering
    sortKey = flip (comparing (outVal . snd))

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
       => PrivacyMode -> InputSelectionPolicy h a m
random privacyMode utxo = \goals -> runInputPolicyT utxo $
    mconcat <$> mapM go goals
  where
    go :: Output a -> InputPolicyT h a m PartialTxStats
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
        ipsSelectedInputs   %= Set.union (utxoDomain selected)
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
              => TargetRange -> InputPolicyT h a m (Utxo h a)
randomInRange AtLeast{..} = go 0 utxoEmpty
  where
    -- Invariant:
    --
    -- > acc == utxoBalance selected
    go :: Value -> Utxo h a -> InputPolicyT h a m (Utxo h a)
    go acc selected
      | acc >= targetMin = return selected
      | otherwise        = do io@(_, out) <- findRandomOutput
                              go (acc + outVal out) (utxoInsert io selected)
randomInRange InRange{..} = go 0 utxoEmpty
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
    go :: Value -> Utxo h a -> InputPolicyT h a m (Utxo h a)
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
              selected' = utxoInsert (i, o) selected
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
findRandomOutput :: LiftQuickCheck m => InputPolicyT h a m (Input h a, Output a)
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
                    -> InputPolicyT h a m (Maybe (Input h a, Output a))
tryFindRandomOutput p = do
    utxo <- utxoToMap <$> use ipsUtxo
    mIO  <- liftQuickCheck $ randomElement utxo
    case mIO of
      Nothing       -> return Nothing
      Just (io, utxo')
        | p io      -> do ipsUtxo .= utxoFromMap utxo' ; return $ Just io
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
