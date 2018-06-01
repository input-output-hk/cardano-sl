{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE ViewPatterns               #-}

module InputSelection.Policy (
    -- * Infrastructure
    LiftQuickCheck(..)
  , RunPolicy(..)
  , PrivacyMode(..)
  , InputSelectionPolicy
  , InputSelectionFailure (..)
  , HasTreasuryAddress (..)
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
import           Formatting (sformat, shown, (%))
import           Test.QuickCheck

import           Cardano.Wallet.Kernel.CoinSelection.Types (ExpenseRegulation (..))

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

class Eq a => HasTreasuryAddress a where
    treasuryAddr :: a

instance HasTreasuryAddress () where
    treasuryAddr = ()

-- | Monads in which we can run input selection policies
class Monad m => RunPolicy m a | m -> a where
  -- | Generate change address
  genChangeAddr :: m a

  -- | Generate fresh hash
  genFreshHash :: m Int

data InputSelectionFailure a = InputSelectionFailure
                             -- ^ A generic failure
                             | InsufficientFundsToCoverFee ExpenseRegulation Value (Output a)
                             -- ^ We need extra funds to cover the fee.

type InputSelectionPolicy h a m =
      (Int -> [Value] -> Value)
      -- ^ Function to estimate the fee
   -> ExpenseRegulation
      -- ^ The expense regulation (i.e. how pays for the fee)
   -> Utxo h a
      -- ^ The initial UTXO
   -> [Output a]
      -- ^ The initial outputs we need to pay.
   -> m (Either [InputSelectionFailure a] (Transaction h a, TxStats))

{-------------------------------------------------------------------------------
  Input selection combinator
-------------------------------------------------------------------------------}

data InputPolicyState h a = InputPolicyState {
      -- | Available entries in the UTxO
      _ipsUtxo           :: Utxo h a

      -- | Selected inputs
    , _ipsSelectedInputs :: Set (Input h a)

      -- | Generated change outputs
    , _ipsChangeOutputs  :: [Output a]
    }

initInputPolicyState :: Utxo h a -> InputPolicyState h a
initInputPolicyState utxo = InputPolicyState {
      _ipsUtxo             = utxo
    , _ipsSelectedInputs   = Set.empty
    , _ipsChangeOutputs = []
    }


-- | Merges two 'InputPolicyState' together.
-- The final 'Utxo' is taken from the last passed as input, as we need to
-- pick the \"most recent\" one.
mergeInputPolicyState :: Hash h a
                      => InputPolicyState h a
                      -> InputPolicyState h a
                      -> InputPolicyState h a
mergeInputPolicyState (InputPolicyState _ s1 c1) (InputPolicyState u2 s2 c2) =
    InputPolicyState u2 (s1 `Set.union` s2) (c1 `mappend` c2)

makeLenses ''InputPolicyState

newtype InputPolicyT h a m x = InputPolicyT {
      unInputPolicyT :: StateT (InputPolicyState h a) (ExceptT [InputSelectionFailure a] m) x
    }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadState (InputPolicyState h a)
           , MonadError [InputSelectionFailure a]
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

type RunPolicyResult a r = Either [InputSelectionFailure a] r

newtype TotalOutput = TotalOutput { getTotal :: Value }


runInputPolicyT :: forall h a m. (HasTreasuryAddress a, Hash h a, RunPolicy m a)
                => (Int -> [Value] -> Value)
                -- ^ A function to estimate the fee.
                -> ExpenseRegulation
                -- ^ Who pays the fees, if the sender or the receivers.
                -> Utxo h a
                -- ^ The original UTXO.
                -> [Output a]
                -- ^ The original outputs we need to pay to.
                -> ([Output a] -> InputPolicyT h a m PartialTxStats)
                -- ^ The input policy
                -> m (RunPolicyResult a (Transaction h a, TxStats))
runInputPolicyT estimateFee expenseRegulation originalUtxo originalOutputs policyT = do
     mx <- runExceptT (runStateT (unInputPolicyT (policyT originalOutputs)) initSt)
     case mx of
       Left errs ->
         return $ Left errs
       Right (ptxStats, finalSt) -> do
         let changeOutputs  = finalSt ^. ipsChangeOutputs
             inputsLen      = length (finalSt ^. ipsSelectedInputs)
             allOutputs     = changeOutputs <> originalOutputs
             upperBoundFee  = estimateFee inputsLen (map outVal allOutputs)

         -- Regulates the inputs & outputs based on the 'ExpenseRegulation' and
         -- an upper bound fee. For 'ReceiverPaysFee' it's quite simple as the
         -- number of inputs will never increase (because the receivers are paying
         -- for the fee). For the 'SenderPaysFee' the extra cost might not be
         -- covered by the selected inputs alone, to which we react by re-running
         -- the input 'policyT' adding the \"slack\" to cover as the (only) goal
         -- to satisfy.
         case expenseRegulation of
             ReceiverPaysFee ->
                 regulateReceivers (ptxStats, finalSt) upperBoundFee
             SenderPaysFee  ->
                 regulateSender (ptxStats, finalSt) upperBoundFee
  where
    initSt = initInputPolicyState originalUtxo

    -- Calculates the \"slice\" of the fee each sender or receiver has to pay.
    -- Such slice is proportional to the size of the original output in case of
    -- 'ReceiverPaysFee', and proportional to the size of the change outputs in
    -- case of a 'SenderPaysFee'.
    -- NOTE(adn) Not dealing with divisions by 0 here to not complicate the
    -- code too much, but in a real implementation we should have 'NonEmpty (Output a)'
    -- being passed as input.
    epsilon :: Value -> Output a -> TotalOutput -> Value
    epsilon upperBoundFee (outVal -> val) totalOutput =
        let (tf :: Double) = fromIntegral upperBoundFee
        -- Round the fee pessimistically. This might result in receivers paying
        -- a slightly higher fee.
        in case getTotal totalOutput of
               0     -> floor (tf * fromIntegral val)
               total -> floor $ tf * (fromIntegral val / fromIntegral total)

    -- | Calculates the total 'Value' from all the @original@ outputs.
    totalOriginalOutputValue :: TotalOutput
    totalOriginalOutputValue =
        TotalOutput (sum . map outVal $ originalOutputs)

    -- | Calculates the total 'Value' from @all@ the outputs, including
    -- change addresses.
    totalChangeOutputValue :: [Output a] -> TotalOutput
    totalChangeOutputValue changeOutputs =
        TotalOutput (sum . map outVal $ changeOutputs)

    -- | Try regulating an output using the 'ExpenseRegulation' provided as
    -- part of 'runPolicyT'.
    tryRegulate :: Value
                -- ^ The upper bound fee
                -> TotalOutput
                -- ^ Some notion of \"total output\"
                -> Output a
                -- ^ The output we need to regulate
                -> Either (InputSelectionFailure a) (Output a)
    tryRegulate upperBoundFee totalOutput output =
        let original = outVal output
            e        = epsilon upperBoundFee output totalOutput
            amended  = original - e
        in case amended > original of -- We underflowed
               True  -> Left (InsufficientFundsToCoverFee expenseRegulation e output)
               False -> Right (output { outVal = amended })

    -- | Regulates the receivers, by substracting epsilon from their original
    -- 'Output' value.
    regulateReceivers :: (PartialTxStats, InputPolicyState h a)
                      -> Value
                      -> m (RunPolicyResult a (Transaction h a, TxStats))
    regulateReceivers (ptxStats, st) upperBoundFee = do
        case partitionEithers (map (tryRegulate upperBoundFee totalOriginalOutputValue) originalOutputs) of
            ([], amendedOutputs) -> do
                let inputs = st ^. ipsSelectedInputs
                    changeOutputs = st ^. ipsChangeOutputs
                tx <- mkTxFreshHash inputs (amendedOutputs <> changeOutputs) upperBoundFee
                return $ Right (tx, fromPartialTxStats ptxStats)
            (errs, _) -> return (Left errs)

    -- | Regulates the sender, by substracting epsilon from the change
    -- addresses, trying to cover the fee.
    regulateSender :: (PartialTxStats, InputPolicyState h a)
                   -> Value
                   -> m (RunPolicyResult a (Transaction h a, TxStats))
    regulateSender (ptxStats, st) upperBoundFee = do
        res <- tryCoverFee (ptxStats, st) upperBoundFee
        case res of
            Left errs -> return (Left errs)
            Right (finalStats, finalSt, finalFee) -> do
                let totalOutput = totalChangeOutputValue (finalSt ^. ipsChangeOutputs)
                case partitionEithers (map (tryRegulate finalFee totalOutput) (finalSt ^. ipsChangeOutputs)) of
                     ([], amendedChange) -> do
                         -- Filter the change addresses which, after being amended,
                         -- are completely empty.
                         let finalChangeOutputs = filter ((/= 0) . outVal) amendedChange
                         tx <- mkTxFreshHash (finalSt ^. ipsSelectedInputs)
                                             (originalOutputs <> finalChangeOutputs)
                                             finalFee
                         return $ Right (tx, fromPartialTxStats finalStats)
                     (err : _, _) ->
                         -- If we get an error here, by the virtue of the fact
                         -- that by construction the fee should be totally
                         -- covered by the selected inputs, we flag the
                         -- invariant with an exception
                         case err of
                              InsufficientFundsToCoverFee _ e erroredOutput ->
                                 let msg = sformat (("regulateSender: " %
                                                     "failed to regulate the final change outputs! " %
                                                     "{ finalFee = ") % shown %
                                                     ", availableChange = " % shown %
                                                     ", totalOutputValue = " % shown %
                                                     ", epsilon = " % shown %
                                                     ", erroredOutput = " % shown %
                                                     "}")
                                                   finalFee
                                                   (sum $ map outVal $ finalSt ^. ipsChangeOutputs)
                                                   (getTotal totalOriginalOutputValue)
                                                   e
                                                   (outVal erroredOutput)
                                 in error msg
                              _ -> error "regulateSender: tryRegulate fee failed in an unexpected way."

    -- Iteratively try to cover the fee by picking \"one more input\" reusing
    -- the externally-passed policy.
    tryCoverFee :: (PartialTxStats, InputPolicyState h a)
                -> Value
                -> m (RunPolicyResult a (PartialTxStats, InputPolicyState h a, Value))
    tryCoverFee (ptxStats, st) fee = do
        let change = sum . map outVal $ (st ^. ipsChangeOutputs)
        case change >= fee of
            True  -> return $ Right (ptxStats, st, fee)
            False -> do
                let feeAsOutput = Output treasuryAddr fee
                mx <- runExceptT (runStateT (unInputPolicyT (policyT [feeAsOutput])) (initInputPolicyState (st ^. ipsUtxo)))
                case mx of
                  Left errs -> return $ Left errs
                  Right (ptxStats', st') -> do
                      let newState = st `mergeInputPolicyState` st'
                          newPtxStats = ptxStats `mappend` ptxStats'
                          allOutputs  = originalOutputs <> newState ^. ipsChangeOutputs
                          newFee = estimateFee (length $ newState ^. ipsSelectedInputs)
                                               (map outVal allOutputs)
                      tryCoverFee (newPtxStats, newState) newFee

-- | Creates a new 'Transaction' given a 'Set' of inputs, a list of outputs,
-- a fee and a fresh hash.
mkTx :: Set (Input h a) -> [Output a] -> Value -> Int -> Transaction h a
mkTx inputs outputs fee freshHash =
    Transaction {
      trFresh = 0
    , trIns   = inputs
    , trOuts  = outputs
    , trFee   = fee
    , trHash  = freshHash
    , trExtra = []
    }

-- | Like 'mkTx', but runs in the 'RunPolicy' monad and automatically generates
-- a fresh hash.
mkTxFreshHash :: RunPolicy m a
              => Set (Input h a)
              -> [Output a]
              -> Value
              -> m (Transaction h a)
mkTxFreshHash inputs outputs fee = mkTx inputs outputs fee <$> genFreshHash

{-------------------------------------------------------------------------------
  Always find the largest UTxO possible
-------------------------------------------------------------------------------}

largestFirst :: forall h a m. (RunPolicy m a, Hash h a, HasTreasuryAddress a)
             => InputSelectionPolicy h a m
largestFirst estimateFee expenseRegulation utxo goals =
  runInputPolicyT estimateFee expenseRegulation utxo goals largestFirstT

-- | Always use largest UTxO possible
--
-- NOTE: This is a very efficient implementation. Doesn't really matter, this
-- is just for testing; we're not actually considering using such a policy.
largestFirstT :: forall h a m. (RunPolicy m a, Hash h a)
              => [Output a]
              -> InputPolicyT h a m PartialTxStats
largestFirstT goals = mconcat <$> mapM go goals
  where
    go :: Output a -> InputPolicyT h a m PartialTxStats
    go (Output _a val) = do
        sorted   <- sortBy sortKey . utxoToList <$> use ipsUtxo
        selected <- case select sorted utxoEmpty 0 of
                      Nothing -> throwError [InputSelectionFailure]
                      Just u  -> return u

        ipsUtxo             %= utxoRemoveInputs (utxoDomain selected)
        ipsSelectedInputs   %= Set.union (utxoDomain selected)

        let selectedSum = utxoBalance selected
            change      = selectedSum - val

        unless (change == 0) $ do
          changeAddr <- genChangeAddr
          ipsChangeOutputs %= (Output changeAddr change :)

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

random :: forall h a m. (LiftQuickCheck m, RunPolicy m a, Hash h a, HasTreasuryAddress a)
       => PrivacyMode
       -> InputSelectionPolicy h a m
random privacyMode estimateFee expenseRegulation utxo goals =
  runInputPolicyT estimateFee expenseRegulation utxo goals (randomT privacyMode)

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
randomT :: forall h a m. (RunPolicy m a, LiftQuickCheck m, Hash h a)
        => PrivacyMode
        -> [Output a]
        -> InputPolicyT  h a m PartialTxStats
randomT privacyMode goals = mconcat <$> mapM go goals
  where
    go :: Output a -> InputPolicyT h a m PartialTxStats
    go (Output _a val) = do
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
        let selectedSum = utxoBalance selected
            change      = selectedSum - val
        unless (change == 0) $ do
          changeAddr <- genChangeAddr
          ipsChangeOutputs %= (Output changeAddr change :)
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
            | otherwise         -> throwError [InputSelectionFailure]
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
      Nothing -> throwError [InputSelectionFailure]

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
