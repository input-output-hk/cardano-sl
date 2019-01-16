{-# LANGUAGE BangPatterns #-}

module Cardano.Wallet.Kernel.CoinSelection.Generic.Fees
    ( ExpenseRegulation(..)
    , FeeOptions(..)
    , CoinSelFinalResult(..)
    , adjustForFees
    ) where

import           Universum

import           Control.Monad.Trans.Except (Except)
import           Formatting (bprint)
import           Formatting.Buildable (Buildable (..))

import           Cardano.Wallet.Kernel.CoinSelection.Generic

{-------------------------------------------------------------------------------
  Top-level API
-------------------------------------------------------------------------------}

data ExpenseRegulation =
      SenderPaysFee
    -- ^ The sender pays for the fee. This is the typical case.
    | ReceiverPaysFee
    -- ^ The receiver pays for the fee. This is useful for cases
    -- where users wants to transfer funds between wallets owned by them,
    -- and they wish to trasfer an @exact@ amount (or, for example, the max
    -- amount).

data FeeOptions dom = FeeOptions
    { foEstimate          :: Int -> [Value dom] -> Fee dom
      -- ^ Estimate fees based on number of inputs and values of the outputs
    , foExpenseRegulation :: ExpenseRegulation
      -- ^ Expense regulation (who pays the fees?)
    , foDustThreshold     :: Value dom
      -- ^ Change addresses below the given threshold will be evicted
      -- from the created transaction. If you only want to remove change
      -- outputs equal to 0, set 'csoDustThreshold' to 0.
    }

data CoinSelFinalResult dom = CoinSelFinalResult
    { csrInputs  :: NonEmpty (UtxoEntry dom)
    -- ^ Picked inputs
    , csrOutputs :: NonEmpty (Output dom)
    -- ^ Picked outputs
    , csrChange  :: [Value dom]
    -- ^ Resulting changes
    }

type PickUtxo m utxo
    =  Value (Dom utxo)
    -> CoinSelT utxo CoinSelHardErr m (Maybe (UtxoEntry (Dom utxo)))

-- | Given the coin selection result from a policy run, adjust the outputs
-- for fees, potentially returning additional inputs that we need to cover
-- all fees.
-- We lose the relationship between the transaction outputs and their
-- corresponding inputs/change outputs here. This is a decision we
-- may wish to revisit later. For now however note that since
--
--      (a) coin selection tries to establish a particular ratio
--          between payment outputs and change outputs (currently it
--          aims for an average of 1:1)
--
--      (b) coin selection currently only generates a single change
--          output per payment output, distributing the fee
--          proportionally across all change outputs is roughly
--          equivalent to distributing it proportionally over the
--          payment outputs (roughly, not exactly, because the 1:1
--          proportion is best effort only, and may in some cases be
--          wildly different).
--
-- Note that for (a) we don't need the ratio to be 1:1, the above
-- reasoning will remain true for any proportion 1:n. For (b) however,
-- if coin selection starts creating multiple outputs, and this number
-- may vary, then losing the connection between outputs and change
-- outputs will mean that that some outputs may pay a larger
-- percentage of the fee (depending on how many change outputs the
-- algorithm happened to choose).
adjustForFees
    :: forall utxo m. (Monad m, CoinSelDom (Dom utxo))
    => FeeOptions (Dom utxo)
    -> PickUtxo m utxo
    -> [CoinSelResult (Dom utxo)]
    -> CoinSelT utxo CoinSelHardErr m (CoinSelFinalResult (Dom utxo))
adjustForFees feeOptions pickUtxo css = do
    let inps = concatMap (selectedEntries . coinSelInputs) css
    let outs = map coinSelOutput css
    let chgs = concatMap coinSelChange css

    (inps', outs', chgs') <-
        case foExpenseRegulation feeOptions of
          ReceiverPaysFee ->
            coinSelLiftExcept $ receiverPaysFee feeOptions inps outs chgs

          SenderPaysFee ->
            senderPaysFee pickUtxo feeOptions inps outs chgs

    let estimatedFee = getFee $ feeUpperBound feeOptions inps outs chgs
    let actualFee = getFee $ computeFee inps' outs' chgs'
    -- NOTE
    -- We enforce the following invariant:
    --
    --   estimatedFee < actualFee < 2 * estimatedFee
    --
    -- This coefficient (2*...) is mostly taken out of nowhere, but if anything
    -- go beyond that upper bound, we would know that our algorithm for fee
    -- reconciliation below is messed up.
    -- Similarly, the algorithm tries to take money from inputs until it reaches
    -- the goal fixed by 'estimatedFee'. So, the actualFee just can't be lower
    -- than the goal.
    --
    -- (PS: using `valueDiv` instead of `valueMul` to avoid overflow)
    if (actualFee < estimatedFee || actualFee `valueDiv` 2 > estimatedFee) then
        error $ "adjustForFees: fee out of bounds: " <> pretty actualFee <> " while expecting ~" <> pretty estimatedFee
    else do
        let neInps = case inps' of
                []   -> error "adjustForFees: empty list of inputs"
                i:is -> i :| is

        let neOuts = case outs' of
                []   -> error "adjustForFees: empty list of outputs"
                o:os -> o :| os

        return $ CoinSelFinalResult neInps neOuts chgs'


{-------------------------------------------------------------------------------
  Receiver pays fee
-------------------------------------------------------------------------------}

receiverPaysFee
    :: forall dom. CoinSelDom dom
    => FeeOptions dom
    -> [UtxoEntry dom]
    -> [Output dom]
    -> [Value dom]
    -> Except CoinSelHardErr ([UtxoEntry dom], [Output dom], [Value dom])
receiverPaysFee feeOptions inps outs chgs = do
    let totalFee = feeUpperBound feeOptions inps outs chgs
    outs' <- mapM go . divvyFee outVal totalFee $ outs
    return (inps, outs', chgs)
  where
    go
        :: (Fee dom, Output dom)
        -> Except CoinSelHardErr (Output dom)
    go (fee, out) =
        case outSubFee fee out of
          Just newOut ->
            return newOut
          Nothing ->
            throwError $
              CoinSelHardErrOutputCannotCoverFee (pretty out) (pretty fee)

{-------------------------------------------------------------------------------
  Sender pays fee
-------------------------------------------------------------------------------}

senderPaysFee
    :: forall utxo m. (Monad m, CoinSelDom (Dom utxo))
    => PickUtxo m utxo
    -> FeeOptions (Dom utxo)
    -> [UtxoEntry (Dom utxo)]
    -> [Output (Dom utxo)]
    -> [Value (Dom utxo)]
    -> CoinSelT utxo CoinSelHardErr m ([UtxoEntry (Dom utxo)], [Output (Dom utxo)], [Value (Dom utxo)])
senderPaysFee pickUtxo feeOptions = go
  where
    removeDust :: [Value (Dom utxo)] -> [Value (Dom utxo)]
    removeDust = changesRemoveDust (foDustThreshold feeOptions)

    go inps outs chgs = do
        -- 1/
        -- We compute fees using all inputs, outputs and changes since
        -- all of them have an influence on the fee calculation.
        let fee = feeUpperBound feeOptions inps outs chgs

        -- 2/ Substract fee from all change outputs, proportionally to their value.
        let (chgs', remainingFee) = reduceChangeOutputs removeDust fee chgs

        -- 3.1/
        -- Should the change cover the fee, we're (almost) good. By removing
        -- change outputs, we make them smaller and may reduce the size of the
        -- transaction, and the fee. Thus, we end up paying slightly more than
        -- the upper bound. We could do some binary search and try to
        -- re-distribute excess across changes until fee becomes bigger.
        if getFee remainingFee == valueZero then do
            return (inps, outs, chgs')

        -- 3.2/
        -- Otherwise, we need an extra entries from the available utxo to
        -- cover what's left. Note that this entry may increase our change
        -- because we may not consume it entirely. So we will just split
        -- the extra change across all changes possibly increasing the
        -- number of change outputs (if there was none, or if increasing
        -- a change value causes an overflow).
        --
        -- Because selecting a new input increases the fee, we need to
        -- re-run the algorithm with this new elements and using the initial
        -- change plus the extra change brought up by this entry and see if
        -- we can now correctly cover fee.
        else do
            extraUtxo <- coverRemainingFee pickUtxo remainingFee
            let inps'       = selectedEntries extraUtxo
            let extraChange = splitChange (selectedBalance extraUtxo) chgs
            go (inps <> inps') outs extraChange


coverRemainingFee
    :: forall utxo m. (Monad m, CoinSelDom (Dom utxo))
    => PickUtxo m utxo
    -> Fee (Dom utxo)
    -> CoinSelT utxo CoinSelHardErr m (SelectedUtxo (Dom utxo))
coverRemainingFee pickUtxo fee = go emptySelection
  where
    go :: SelectedUtxo (Dom utxo)
       -> CoinSelT utxo CoinSelHardErr m (SelectedUtxo (Dom utxo))
    go !acc
      | selectedBalance acc >= getFee fee =
          return acc
      | otherwise = do
          mio <- (pickUtxo $ unsafeValueSub (getFee fee) (selectedBalance acc))
          io  <- maybe (throwError CoinSelHardErrCannotCoverFee) return mio
          go (select io acc)


-- Equally split the extra change obtained when picking new inputs across all
-- other change. Note that, it may create an extra change output if:
--
--   (a) There's no change at all initially
--   (b) Adding change to an exiting one would cause an overflow
--
-- It makes no attempt to divvy the new output proportionally over the change
-- outputs. This means that if we happen to pick a very large UTxO entry, adding
-- this evenly rather than proportionally might skew the payment:change ratio a
-- lot. Could consider defining this in terms of divvy instead.
splitChange
    :: forall dom. (CoinSelDom dom)
    => Value dom
    -> [Value dom]
    -> [Value dom]
splitChange = go
  where
    go remaining as | remaining == valueZero =
        as
    go remaining [] = [remaining]
        -- we only create new change if for whatever reason there is none already
        -- or if is some overflow happens when we try to add.
    go remaining [a] = case valueAdd remaining a of
        Just newChange -> [newChange]
        Nothing        -> [a, remaining]
    go remaining ls@(a : as) =
      let piece = valueDiv remaining (length ls)
          newRemaining = unsafeValueSub remaining piece -- unsafe because of div.
      in case valueAdd piece a of
          Just newChange -> newChange : go newRemaining as
          Nothing        -> a : go remaining as


-- | Reduce the given change outputs by the total fee, returning the remainig
-- change outputs if any are left, or the remaining fee otherwise
--
-- As for the overall fee in 'feeFromChange', we divvy up the fee over all
-- change outputs proportionally, to try and keep any output:change ratio
-- as unchanged as possible
reduceChangeOutputs
    :: forall dom. CoinSelDom dom
    => ([Value dom] -> [Value dom])
    -> Fee dom
    -> [Value dom]
    -> ([Value dom], Fee dom)
reduceChangeOutputs removeDust totalFee cs =
    case divvyFeeSafe identity totalFee cs of
        Nothing ->
            (removeDust cs, totalFee)
        Just xs ->
            bimap removeDust unsafeFeeSum
            . unzip
            . map go
            $ xs
  where
    -- Reduce single change output, returning remaining fee
    go :: (Fee dom, Value dom) -> (Value dom, Fee dom)
    go (fee, change)
      | change >= getFee fee =
          (unsafeValueSub change (getFee fee), Fee valueZero)
      | otherwise =
          (valueZero, adjustFee (`unsafeValueSub` change) fee)

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

feeUpperBound
    :: forall dom. (CoinSelDom dom)
    => FeeOptions dom
    -> [UtxoEntry dom]
    -> [Output dom]
    -> [Value dom]
    -> Fee dom
feeUpperBound FeeOptions{..} inps outs chgs =
    foEstimate numInputs outputs
  where
    numInputs = fromIntegral $ sizeToWord $ selectedSize $ foldl' (flip select) emptySelection inps
    outputs = map outVal outs <> chgs

-- Computing actual fee is a bit tricky in the generic realm because we don't
-- know what type representation is used by the underlying implementation. So,
-- we can't just sum up all the input and substract the sub of all outputs
-- (incl. change) because we'll risk an overflow with each sum. Instead, we
-- reduce the input value iteratively, coin by coin using a safe distance
-- between coins that are known to be within bounds.
-- The algorithm converge because we know that by construction, there are less
-- outputs than inputs. In essence, this computes:
--
--     fees = ∑ inputs - (∑ outputs + ∑ changes)
computeFee
    :: forall dom. (CoinSelDom dom)
    => [UtxoEntry dom]
    -> [Output dom]
    -> [Value dom]
    -> Fee dom
computeFee inps outs chgs =
    Fee $ collapse (map utxoEntryVal inps) (filter (> valueZero) $ map outVal outs <> chgs)
  where
    invariantViolation msg = error $ "invariant violation: " <> msg
        <> "\n  inps: " <> show (pretty . utxoEntryOut <$> inps)
        <> "\n  outs: " <> show (pretty <$> outs)
        <> "\n  chgs: " <> show (pretty <$> chgs)

    -- Some remaining inputs together. At this point, we've removed
    -- all outputs and changes, so what's left are simply the actual fees.
    -- It's unrealistic to imagine them being bigger than the max coin value.
    collapse plus [] = case valueSum plus of
        Nothing -> invariantViolation "fees are bigger than max coin value"
        Just a  -> a

    -- In order to safely compute fees at this level, we need make sure we don't
    -- overflow. Therefore, we remove outputs to inputs until there's no outputs
    -- left to remove.
    collapse (p:ps) (m:ms)
        | p > m     = let p' = valueDist p m in collapse (p':ps) ms
        | p < m     = let m' = valueDist p m in collapse ps (m':ms)
        | otherwise = collapse ps ms

    -- This branch can only happens if we've depleted all our inputs and there
    -- are still some outputs left to remove from them. If means the total value
    -- of outputs (incl. change) was bigger than the total input value which is
    -- by definition, impossible; unless we messed up real hard.
    collapse []  _ = invariantViolation "outputs are bigger than inputs"

-- | divvy fee across outputs, discarding zero-output if any. Returns `Nothing`
-- when there's no more outputs after filtering, in which case, we just can't
-- divvy fee.
divvyFeeSafe
    :: forall dom a. CoinSelDom dom
    => (a -> Value dom)
    -> Fee dom
    -> [a]
    -> Maybe [(Fee dom, a)]
divvyFeeSafe f fee as = case filter ((/= valueZero) . f) as of
    []  -> Nothing
    as' -> Just (divvyFee f fee as')

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

instance Buildable ExpenseRegulation where
    build SenderPaysFee   = bprint "SenderPaysFee"
    build ReceiverPaysFee = bprint "ReceiverPaysFee"
