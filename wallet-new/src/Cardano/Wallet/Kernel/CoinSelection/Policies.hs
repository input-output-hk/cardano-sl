{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns               #-}
module Cardano.Wallet.Kernel.CoinSelection.Policies (
      defaultPolicy
    , utxoBalance
    ) where

import           Universum

import           Cardano.Wallet.Kernel.CoinSelection.Types (Addr (Treasury), CoinPolicyState (..),
                                                            CoinSelectionFailure (..),
                                                            CoinSelectionOptions (..),
                                                            ExpenseRegulation (..), Output (..),
                                                            RunPolicy (..), RunPolicyResult,
                                                            TotalOutput (..), cpsChangeOutputs,
                                                            cpsSelectedInputs, cpsUtxo,
                                                            csoDustThreshold, csoEstimateFee,
                                                            csoExpenseRegulation, csoMakeSigner,
                                                            fromTxOut, fromTxOutAux,
                                                            initCoinPolicyState,
                                                            mergeCoinPolicyState, toTxOutAux)

import           Control.Lens ((%=), (.=))
import           Control.Monad.Except (MonadError (..))
import           Crypto.Random (MonadRandom)
import           Formatting (sformat, shown, (%))
import           Pos.Crypto.Random (randomNumber)

import qualified Data.List
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Pos.Client.Txp.Util as Core
import qualified Pos.Core as Core
import qualified Pos.Txp as Core

import           Cardano.Wallet.Kernel.Util (restrictKeys, withoutKeys)

{-------------------------------------------------------------------------------
  Basic types
-------------------------------------------------------------------------------}

-- TODO(adn) Use 'StrictStateT' from Edsko.
newtype CoinPolicyT a m x = CoinPolicyT {
      unCoinPolicyT :: StateT (CoinPolicyState a) (ExceptT [CoinSelectionFailure a] m) x
    }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadState (CoinPolicyState a)
             , MonadError [CoinSelectionFailure a]
             )

instance MonadTrans (CoinPolicyT a) where
  lift = CoinPolicyT . lift . lift

instance RunPolicy m a => RunPolicy (CoinPolicyT a m) a where
  genChangeAddr = lift genChangeAddr



{-------------------------------------------------------------------------------
  Running a policy
-------------------------------------------------------------------------------}

-- | Runs the coin selection policy running in a 'CoinPolicyT' monad, returning
-- either an error if no coin selection was possible, or a new transaction.
-- N.B. This function is not exported by this module as it's considered internal
-- only. Policy writers should simply write new policy running in a 'CoinPolicyT',
-- export the 'runner' which calls internall 'runCoinPolicyT' and call it a day.
runCoinPolicyT :: forall m. (Core.HasProtocolMagic, RunPolicy m Core.Address)
               => CoinSelectionOptions
               -> Core.Utxo
               -- ^ The original UTXO.
               -> NonEmpty Core.TxOut
               -- ^ The original outputs we need to pay to.
               -> (NonEmpty (Output Core.Address) -> CoinPolicyT Core.Address m ())
               -- ^ The input policy
               -> m (RunPolicyResult Core.Address Core.TxAux)
runCoinPolicyT options originalUtxo goals policyT = do
    -- Fail if any of the output is a redeem address.
    let errs = validateOutputs goals
    case Data.List.null errs of
        True -> do
            let estimateFee = options ^. csoEstimateFee
            mx <- runExceptT (execStateT (unCoinPolicyT (policyT originalOutputs)) initSt)
            case mx of
                Left errs -> return $ Left errs
                Right finalSt -> do
                    let changeOutputs  = finalSt ^. cpsChangeOutputs
                        inputsLen      = length (finalSt ^. cpsSelectedInputs)
                        allOutputs     = originalOutputs `appendChange` changeOutputs
                        upperBoundFee  = estimateFee inputsLen (map outVal allOutputs)
                    -- Regulates the inputs & outputs based on the 'ExpenseRegulation' and
                    -- an upper bound fee. For 'ReceiverPaysFee' it's quite simple as the
                    -- number of inputs will never increase (because the receivers are paying
                    -- for the fee). For the 'SenderPaysFee' the extra cost might not be
                    -- covered by the selected inputs alone, to which we react by re-running
                    -- the input 'policyT' adding the \"slack\" to cover as the (only) goal
                    -- to satisfy.
                    case options ^. csoExpenseRegulation of
                        ReceiverPaysFee -> regulateReceivers finalSt upperBoundFee
                        SenderPaysFee   -> regulateSender finalSt upperBoundFee
        False -> return (Left errs)
    where
        initSt = initCoinPolicyState originalUtxo

        originalOutputs :: NonEmpty (Output Core.Address)
        originalOutputs = map fromTxOut goals

        -- Calculates the \"slice\" of the fee each sender or receiver has to pay.
        -- Such slice is proportional to the size of the original output in case of
        -- 'ReceiverPaysFee', and proportional to the size of the change outputs in
        -- case of a 'SenderPaysFee'.
        epsilon :: Core.Coin -> Output Core.Address -> TotalOutput -> Core.Coin
        epsilon upperBoundFee (Core.getCoin . outVal -> val) totalOutput =
            let (tf :: Double) = fromIntegral (Core.getCoin upperBoundFee)
            -- Round the fee pessimistically. This might result in receivers paying
            -- a slightly higher fee.
            in case Core.getCoin (getTotal totalOutput) of
                   0     -> upperBoundFee -- pay the fee entirely
                   total ->
                     let (weight :: Double) = fromIntegral val / fromIntegral total
                     -- Overflowing here shouldn't be possible as the result
                     -- of the division will be < 1.0, and thus the result will
                     -- always be < of the original tf.
                     in Core.mkCoin (floor $ tf * weight)

        -- | Calculates the total 'Core.Coin' from all the @original@ outputs.
        totalOriginalOutputValue :: TotalOutput
        totalOriginalOutputValue =
            TotalOutput (Core.unsafeIntegerToCoin . Core.sumCoins . map outVal $ originalOutputs)

        -- | Calculates the total 'Core.Coin' from @all@ the outputs, including
        -- change addresses.
        totalChangeOutputValue :: [Output Core.Address] -> TotalOutput
        totalChangeOutputValue changeOutputs =
            TotalOutput (Core.unsafeIntegerToCoin . Core.sumCoins . map outVal $  changeOutputs)

        -- | Try regulating an output using the 'ExpenseRegulation' provided as
        -- part of 'runPolicyT'.
        tryRegulate :: Core.Coin
                    -- ^ The upper bound fee
                    -> TotalOutput
                    -- ^ Some notion of \"total output\"
                    -> Output Core.Address
                    -- ^ The output we need to regulate
                    -> Either (CoinSelectionFailure Core.Address) (Output Core.Address)
        tryRegulate upperBoundFee totalOutput output =
            let original = outVal output
                e        = epsilon upperBoundFee output totalOutput
                amended  = original `Core.subCoin` e
            in case amended of -- Check if we underflowed
                   Nothing -> Left (InsufficientFundsToCoverFee (options ^. csoExpenseRegulation) e output)
                   Just a  -> Right (output { outVal = a })

        -- | Regulates the receivers, by substracting epsilon from their original
        -- 'Output' value.
        regulateReceivers :: Core.HasProtocolMagic
                          => CoinPolicyState Core.Address
                          -- ^ The current internal state.
                          -> Core.Coin
                          -- ^ The fee.
                          -> m (RunPolicyResult Core.Address Core.TxAux)
        regulateReceivers st upperBoundFee = do
            case partitionEithers (map (tryRegulate upperBoundFee totalOriginalOutputValue) (toList originalOutputs)) of
                ([], []) ->
                  error $ "regulateReceivers violated an invariant: when passed a NonEmpty collection, and in presence " <>
                          "of no errors after tryRegulate, the amendedOutputs were empty."
                ([], a : as) -> do
                    let amendedOutputs = a :| as
                        inputs = st ^. cpsSelectedInputs
                        changeOutputs = st ^. cpsChangeOutputs
                    let tx = mkTx options
                                  inputs
                                  (fmap toTxOutAux $ amendedOutputs `appendChange` changeOutputs)
                                  originalUtxo
                    return $ Right tx
                (errs, _) -> return (Left errs)

        -- | Regulates the sender, by substracting epsilon from the change
        -- addresses, trying to cover the fee.
        regulateSender :: CoinPolicyState Core.Address
                       -- ^ The internal state.
                       -> Core.Coin
                       -- ^ The fee.
                       -> m (RunPolicyResult Core.Address Core.TxAux)
        regulateSender st upperBoundFee = do
            res <- tryCoverFee st upperBoundFee
            case res of
                Left errs -> return (Left errs)
                Right (finalSt, finalFee) -> do
                    let totalOutput = totalChangeOutputValue (finalSt ^. cpsChangeOutputs)
                    case partitionEithers (map (tryRegulate finalFee totalOutput) (finalSt ^. cpsChangeOutputs)) of
                         ([], amendedChange) -> do
                             -- Filter the change addresses which, after being amended,
                             -- are completely empty.
                             let dust = maybe 0 Core.getCoin (options ^. csoDustThreshold)
                             let finalChangeOutputs = filter ((>= dust) . Core.getCoin . outVal) amendedChange
                                 tx = mkTx options
                                           (finalSt ^. cpsSelectedInputs)
                                           (fmap toTxOutAux $ originalOutputs `appendChange` finalChangeOutputs)
                                           originalUtxo
                             return $ Right tx
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
                                                       (Core.unsafeIntegerToCoin . Core.sumCoins $ map outVal $ finalSt ^. cpsChangeOutputs)
                                                       (getTotal totalOriginalOutputValue)
                                                       e
                                                       (outVal erroredOutput)
                                     in error msg
                                  _ -> error "regulateSender: tryRegulate fee failed in an unexpected way."

        -- Iteratively try to cover the fee by picking \"one more input\" reusing
        -- the externally-passed policy.
        tryCoverFee :: CoinPolicyState Core.Address
                    -> Core.Coin
                    -> m (RunPolicyResult Core.Address (CoinPolicyState Core.Address, Core.Coin))
        tryCoverFee st fee = do
            let change = Core.unsafeIntegerToCoin . Core.sumCoins . map outVal $ (st ^. cpsChangeOutputs)
            case change >= fee of
                True  -> return $ Right (st, fee)
                False -> do
                    let newGoal = (Output Treasury fee) :| []
                    mx <- runExceptT (execStateT (unCoinPolicyT (policyT newGoal)) (initCoinPolicyState (st ^. cpsUtxo)))
                    case mx of
                        Left errs -> return $ Left errs
                        Right st' -> do
                            let estimateFee = options ^. csoEstimateFee
                                newState = st `mergeCoinPolicyState` st'
                                allOutputs  = originalOutputs `appendChange` (newState ^. cpsChangeOutputs)
                                newFee = estimateFee (length $ newState ^. cpsSelectedInputs)
                                                     (fmap outVal allOutputs)
                            tryCoverFee newState newFee

-- | Assess the input \"goals\" (i.e. the 'TxOut') are @not@ redeem addresses,
-- as it's not possible to pay into a redeem 'Address'.
validateOutputs :: NonEmpty Core.TxOut -> [CoinSelectionFailure Core.Address]
validateOutputs = go mempty . toList
    where
        go :: [CoinSelectionFailure Core.Address]
           -> [Core.TxOut]
           -> [CoinSelectionFailure Core.Address]
        go !acc [] = acc
        go !acc (x:xs) =
            let addr = Core.txOutAddress x
            in if Core.isRedeemAddress addr
                  then go ((OutputIsReedeemAddress addr) : acc) xs
                  else go acc xs


appendChange :: NonEmpty (Output a) -> [Output a] -> NonEmpty (Output a)
appendChange (e :| es) xs = e :| (es <> xs)

utxoEmpty :: Core.Utxo
utxoEmpty = mempty

utxoDomain :: Core.Utxo -> Set Core.TxIn
utxoDomain = Set.fromList . Map.keys

utxoToList :: Core.Utxo -> [(Core.TxIn, Core.TxOutAux)]
utxoToList = Map.toList

utxoRemoveInputs :: Set Core.TxIn -> Core.Utxo -> Core.Utxo
utxoRemoveInputs inps utxo = utxo `withoutKeys` inps

-- | Computes the balance for this 'Utxo'. We use 'unsafeAddCoin' as
-- as long as the 'maxCoinVal' stays within the 'Word64' 'maxBound', the
-- circulating supply of coins is finite and as such we should never have
-- to sum an 'Utxo' which would exceed the bounds.
-- If it does, this is clearly a bug and we throw an 'ErrorCall' exception
-- (crf. 'unsafeAddCoin' implementation).
utxoBalance :: Core.Utxo -> Core.Coin
utxoBalance = foldl' updateFn (Core.mkCoin 0) . Map.elems
    where
        updateFn :: Core.Coin -> Core.TxOutAux -> Core.Coin
        updateFn acc txOutAux =
            acc `Core.unsafeAddCoin` (toCoin txOutAux)

-- | Restricts the 'Utxo' to only the selected set of inputs.
utxoRestrictToInputs :: Set Core.TxIn -> Core.Utxo -> Core.Utxo
utxoRestrictToInputs inps utxo = utxo `restrictKeys` inps

-- | TODO(adn): Should we worry about redemption?
mkTx :: Core.HasProtocolMagic
     => CoinSelectionOptions
     -- ^ The coin selection options.
     -> Set Core.TxIn
     -- ^ The selected inputs.
     -> NonEmpty Core.TxOutAux
     -- ^ The outputs we need to pay to @and@ any generated change output.
     -> Core.Utxo
     -- ^ The original utxo.
     -> Core.TxAux
     -- ^ The generated transaction.
mkTx options inputs outputs utxo =
    -- If this fails, it's genuinely a invariant violation as the input
    -- selection is supposed to actually @pick@ some inputs to begin with.
    let ownedInputs =
         case nonEmpty $ Map.toList (utxoRestrictToInputs inputs utxo) of
             Nothing   -> error "mkTx: invariant violated, the returned utxo was empty."
             Just inps -> fmap repack inps
    in either absurd identity $
        Core.makeMPubKeyTxAddrs (options ^. csoMakeSigner) ownedInputs outputs
    where
        -- Repack a utxo-derived tuple into a format suitable for
        -- 'TxOwnedInputs'.
        repack :: (Core.TxIn, Core.TxOutAux) -> (Core.TxOut, Core.TxIn)
        repack (txIn, aux) = (Core.toaOut aux, txIn)

-- | Gets the underlying value (as a 'Coin') from a 'TxOutAux'.
toCoin :: Core.TxOutAux -> Core.Coin
toCoin = Core.txOutValue . Core.toaOut

{-------------------------------------------------------------------------------
  Always find the largest UTxO possible
-------------------------------------------------------------------------------}

-- | Always use largest UTxO possible
--
-- NOTE: This is a very efficient implementation. Doesn't really matter, this
-- is just for testing; we're not actually considering using such a policy.
largestFirstT :: forall a m. RunPolicy m a
              => NonEmpty (Output a)
              -> CoinPolicyT a m ()
largestFirstT goals = mconcat <$> mapM go (toList goals)
  where
    go :: Output a -> CoinPolicyT a m ()
    go (Output _a val) = do
        sorted   <- sortBy sortKey . utxoToList <$> use cpsUtxo
        selected <- case select sorted utxoEmpty (Core.mkCoin 0) of
                      Nothing -> throwError [CoinSelectionFailure]
                      Just u  -> return u

        cpsUtxo             %= utxoRemoveInputs (utxoDomain selected)
        cpsSelectedInputs   %= Set.union (utxoDomain selected)

        let selectedSum = utxoBalance selected
            -- Underflow here would be a bug, as the 'selectedSum' is
            -- @supposed@ to cover val.
            change      = selectedSum `Core.unsafeSubCoin` val

        unless (Core.getCoin change == 0) $ do
          changeAddr <- genChangeAddr
          cpsChangeOutputs %= (Output changeAddr change :)

        return ()
      where
        select :: [(Core.TxIn, Core.TxOutAux)] -- ^ Sorted available UTxO
               -> Core.Utxo
               -> Core.Coin                    -- ^ Accumulated value
               -> Maybe Core.Utxo
        select _                   acc accSum | accSum >= val = Just acc
        select []                  _   _      = Nothing
        select ((i, o):available') acc accSum =
            select available'
                   (Map.insert i o acc)
                   (accSum `Core.unsafeAddCoin` (toCoin o))

    -- Sort by output value, descending
    sortKey :: (Core.TxIn, Core.TxOutAux) -> (Core.TxIn, Core.TxOutAux) -> Ordering
    sortKey = flip (comparing (toCoin . snd))

{-------------------------------------------------------------------------------
  Random
-------------------------------------------------------------------------------}

data PrivacyMode = PrivacyModeOn | PrivacyModeOff

defaultPolicy :: forall m. (
                Core.HasProtocolMagic
              , MonadRandom m
              , RunPolicy m Core.Address
              )
              => CoinSelectionOptions
              -- ^ User-provided options
              -> Core.Utxo
              -- ^ The initial UTXO
              -> NonEmpty Core.TxOut
              -- ^ The outputs we need to pay.
              -> m (Either [CoinSelectionFailure Core.Address] Core.TxAux)
defaultPolicy options utxo goals =
    runCoinPolicyT options utxo goals (randomT PrivacyModeOn)

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
randomT :: forall a m. (RunPolicy m a, MonadRandom m)
        => PrivacyMode
        -> NonEmpty (Output a)
        -> CoinPolicyT a m ()
randomT privacyMode goals = mconcat <$> mapM go (toList goals)
  where
    go :: Output a -> CoinPolicyT a m ()
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
        cpsSelectedInputs   %= Set.union (utxoDomain selected)

        let selectedSum = utxoBalance selected
            -- Underflow here would be a bug, as the 'selectedSum' is
            -- @supposed@ to cover val.
            change      = selectedSum `Core.unsafeSubCoin` val

        unless (Core.getCoin change == 0) $ do
          changeAddr <- genChangeAddr
          cpsChangeOutputs %= (Output changeAddr change :)
        return ()
      where
        fallback, ideal :: TargetRange
        fallback = AtLeast val

        -- If we overflow or underflow when calculating the 'InRange', we have
        -- no better choice than to go with 'AtLeast'.
        ideal    =
          let range = InRange <$> val `Core.addCoin` (val `Core.divCoin` (2 :: Int))
                              <*> val `Core.addCoin` val
                              <*> (Core.addCoin val =<< (val `Core.mulCoin` (2 :: Int)))
          in fromMaybe fallback range

-- | Target range for picking inputs
data TargetRange =
    -- | Cover at least the specified value, with no upper bound
    AtLeast {
        targetMin :: Core.Coin
      }

    -- | Find inputs in the specified range, aiming for the ideal value
  | InRange {
        targetMin :: Core.Coin
      , targetAim :: Core.Coin
      , targetMax :: Core.Coin
      }

-- | Random input selection: core algorithm
--
-- Select random inputs until we reach a value in the given bounds.
-- Returns the selected outputs.
randomInRange :: forall a m. MonadRandom m
              => TargetRange -> CoinPolicyT a m Core.Utxo
randomInRange AtLeast{..} = go (Core.mkCoin 0) utxoEmpty
  where
    -- Invariant:
    --
    -- > acc == utxoBalance selected
    go :: Core.Coin -> Core.Utxo -> CoinPolicyT a m Core.Utxo
    go acc selected
      | acc >= targetMin = return selected
      | otherwise        = do (i, out) <- findRandomOutput
                              go (acc `Core.unsafeAddCoin` (toCoin out) )
                                 (Map.insert i out selected)
randomInRange InRange{..} = go (Core.mkCoin 0) utxoEmpty
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
    go :: Core.Coin -> Core.Utxo -> CoinPolicyT a m Core.Utxo
    go acc selected = do
        mIO <- tryFindRandomOutput isImprovement
        case mIO of
          Nothing
            | Core.getCoin acc  >= Core.getCoin targetMin -> return selected
            | otherwise                                   -> throwError [CoinSelectionFailure]
          Just (i, o)
            | Core.getCoin acc' >= Core.getCoin targetAim -> return selected'
            | otherwise                                   -> go acc' selected'
            where
              acc'      = acc `Core.unsafeAddCoin` (toCoin o)
              selected' = Map.insert i o selected
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
       isImprovement :: (Core.TxIn, Core.TxOutAux) -> Bool
       isImprovement (_, Core.TxOutAux (Core.TxOut _ val)) =
           case (acc `Core.addCoin` val) of
                Nothing -> False
                Just acc' ->
                    acc' <= targetMax &&
                    distance targetAim acc' < distance targetAim acc

       -- | Calculates the distance between two 'Coin', avoiding underflow.
       distance :: Core.Coin -> Core.Coin -> Core.Coin
       distance a b | Core.getCoin a < Core.getCoin b = Core.mkCoin (Core.getCoin b - Core.getCoin a)
                    | otherwise                       = Core.mkCoin (Core.getCoin a - Core.getCoin b)

-- | Select a random output
findRandomOutput :: MonadRandom m => CoinPolicyT a m (Core.TxIn, Core.TxOutAux)
findRandomOutput = do
    mIO <- tryFindRandomOutput (const True)
    case mIO of
      Just io -> return io
      Nothing -> throwError [CoinSelectionFailure]

-- | Find a random output, and return it if it satisfies the predicate
--
-- If the predicate is not satisfied, state is not changed.
tryFindRandomOutput :: MonadRandom m
                    => ((Core.TxIn, Core.TxOutAux) -> Bool)
                    -> CoinPolicyT a m (Maybe (Core.TxIn, Core.TxOutAux))
tryFindRandomOutput p = do
    utxo <- use cpsUtxo
    mIO  <- randomElement utxo
    case mIO of
      Nothing       -> return Nothing
      Just (io, utxo')
        | p io      -> do cpsUtxo .= utxo' ; return $ Just io
        | otherwise -> return Nothing

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

-- | Pick a random element from a map
--
-- Returns 'Nothing' if the map is empty
randomElement :: forall m k a. MonadRandom m
              => Map k a -> m (Maybe ((k, a), Map k a))
randomElement m
  | Map.null m = return Nothing
  -- The cast is safe as the 'Map.size' is returned as an 'Int' as well.
  | otherwise  = (Just . withIx . fromInteger) <$> randomNumber (toInteger $ Map.size m)
  where
    withIx :: Int -> ((k, a), Map k a)
    withIx ix = (Map.elemAt ix m, Map.deleteAt ix m)
