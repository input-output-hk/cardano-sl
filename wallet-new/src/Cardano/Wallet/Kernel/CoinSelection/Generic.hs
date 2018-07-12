{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE TypeFamilyDependencies     #-}

module Cardano.Wallet.Kernel.CoinSelection.Generic (
    -- * Domain
    IsValue(..)
  , CoinSelDom(..)
  , StandardDom
  , Rounding(..)
  , Fee(..)
  , adjustFee
  , unsafeFeeSum
  , utxoEntryVal
  , sizeOfEntries
  , unsafeValueAdd
  , unsafeValueSub
  , unsafeValueSum
  , unsafeValueAdjust
    -- * Addresses
  , HasAddress(..)
  , utxoEntryAddr
    -- * Monad
  , CoinSelT -- opaque
  , coinSelLiftExcept
  , mapCoinSelErr
  , mapCoinSelUtxo
  , unwrapCoinSelT
  , wrapCoinSelT
    -- * Errors
  , CoinSelHardErr(..)
  , CoinSelSoftErr(..)
  , CoinSelErr(..)
  , catchJustSoft
    -- * Policy
  , CoinSelPolicy
    -- * Coin selection result
  , CoinSelResult(..)
  , defCoinSelResult
  , coinSelInputSet
  , coinSelInputSize
  , coinSelOutputs
  , coinSelRemoveDust
  , coinSelPerGoal
    -- * Generalization over UTxO representations
  , StandardUtxo
  , PickFromUtxo(..)
    -- * Defining policies
  , SelectedUtxo(..)
  , emptySelection
  , select
  , selectedInputs
    -- * Helper functions
  , mapRandom
  , nLargestFromMapBy
  , nLargestFromListBy
  , divvyFee
    -- * Convenience re-exports
  , MonadError(..)
  , MonadRandom
  , withoutKeys
  ) where

import           Universum

import           Control.Monad.Except (Except, MonadError (..))
import           Crypto.Number.Generate (generateBetween)
import           Crypto.Random (MonadRandom (..))
import           Data.Coerce (coerce)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Formatting (bprint, build, (%))
import qualified Formatting.Buildable
import           Test.QuickCheck (Arbitrary (..))

import           Cardano.Wallet.Kernel.Util (withoutKeys)
import           Cardano.Wallet.Kernel.Util.StrictStateT

{-------------------------------------------------------------------------------
  Abstract domain
-------------------------------------------------------------------------------}

data Rounding = RoundUp | RoundDown

class Ord v => IsValue v where
  valueZero   :: v                                   -- ^ @0@
  valueAdd    :: v -> v -> Maybe v                   -- ^ @a + b@
  valueSub    :: v -> v -> Maybe v                   -- ^ @a - b@
  valueDist   :: v -> v -> v                         -- ^ @|a - b|@
  valueRatio  :: v -> v -> Double                    -- ^ @a / b@
  valueAdjust :: Rounding -> Double -> v -> Maybe v  -- ^ @a * b@

class ( Ord (Input dom)
      , IsValue (Value dom)
        -- Buildable and Show instances to aid debugging and testing
      , Buildable (Input  dom)
      , Buildable (Output dom)
      , Buildable (Value  dom)
      , Show (Value dom)
      ) => CoinSelDom dom where
  type Input     dom = i | i -> dom
  type Output    dom = o | o -> dom
  type UtxoEntry dom = e | e -> dom
  type Value     dom = v | v -> dom

  -- | Size of a UTxO
  --
  -- It is important to keep this abstract: when we introduce grouped domains,
  -- then we need to be careful not to accidentally exceed the maximum number
  -- of transaction inputs because we counted the /groups/ of inputs.
  --
  -- We therefore do /not/ want a 'sizeFromInt' method.
  data Size dom :: *

  outVal    :: Output dom -> Value dom
  outSubFee :: Fee dom -> Output dom -> Maybe (Output dom)

  utxoEntryInp :: UtxoEntry dom -> Input  dom
  utxoEntryOut :: UtxoEntry dom -> Output dom

  sizeZero   :: Size dom
  sizeIncr   :: UtxoEntry dom -> Size dom -> Size dom
  sizeToWord :: Size dom -> Word64

  -- default implementations

  default utxoEntryInp :: StandardDom dom => UtxoEntry dom -> Input  dom
  utxoEntryInp = fst

  default utxoEntryOut :: StandardDom dom => UtxoEntry dom -> Output dom
  utxoEntryOut = snd

  default sizeZero :: StandardDom dom => Size dom
  sizeZero = coerce (0 :: Word64)

  default sizeIncr :: StandardDom dom => UtxoEntry dom -> Size dom -> Size dom
  sizeIncr _ = coerce . ((+ 1) :: Word64 -> Word64) . coerce

  default sizeToWord :: StandardDom dom => Size dom -> Word64
  sizeToWord = coerce

-- | Standard domain
class ( CoinSelDom dom
      , UtxoEntry dom ~ (Input dom, Output dom)
      , Coercible (Size dom) Word64
      , Coercible Word64 (Size dom)
      ) => StandardDom dom where

-- | Alias for 'Value'
newtype Fee dom = Fee { getFee :: Value dom }

adjustFee :: (Value dom -> Value dom) -> Fee dom -> Fee dom
adjustFee f = Fee . f . getFee

unsafeFeeSum :: CoinSelDom dom => [Fee dom] -> Fee dom
unsafeFeeSum = Fee . unsafeValueSum . map getFee

utxoEntryVal :: CoinSelDom dom => UtxoEntry dom -> Value dom
utxoEntryVal = outVal . utxoEntryOut

sizeOfEntries :: CoinSelDom dom => [UtxoEntry dom] -> Size dom
sizeOfEntries = foldl' (flip sizeIncr) sizeZero

unsafeValueAdd :: CoinSelDom dom => Value dom -> Value dom -> Value dom
unsafeValueAdd x y = fromMaybe (error "unsafeValueAdd: overflow") $
    valueAdd x y

unsafeValueSub :: CoinSelDom dom => Value dom -> Value dom -> Value dom
unsafeValueSub x y = fromMaybe (error "unsafeValueSub: underflow") $
    valueSub x y

unsafeValueSum :: CoinSelDom dom => [Value dom] -> Value dom
unsafeValueSum = foldl' unsafeValueAdd valueZero

unsafeValueAdjust :: CoinSelDom dom
                  => Rounding -> Double -> Value dom -> Value dom
unsafeValueAdjust r x y = fromMaybe (error "unsafeValueAdjust: out of range") $
    valueAdjust r x y

{-------------------------------------------------------------------------------
  Describing domains which have addresses
-------------------------------------------------------------------------------}

class ( CoinSelDom dom
      , Buildable (Address dom)
      , Ord (Address dom)
      ) => HasAddress dom where
  type Address dom :: *

  outAddr :: Output dom -> Address dom

utxoEntryAddr :: HasAddress dom => UtxoEntry dom -> Address dom
utxoEntryAddr = outAddr . utxoEntryOut


{-------------------------------------------------------------------------------
  Coin selection monad
-------------------------------------------------------------------------------}

-- | Monad that can be uesd to define input selection policies
--
-- NOTE: This stack is carefully defined so that if an error occurs, we do
-- /not/ get a final state value. This means that when we catch errors and
-- provide error handlers, those error handlers will run with the state as it
-- was /before/ the action they wrapped.
newtype CoinSelT utxo e m a = CoinSelT {
      unCoinSelT :: StrictStateT utxo (ExceptT e m) a
    }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadState utxo
           , MonadError e
           )

instance MonadTrans (CoinSelT utxo e) where
  lift = CoinSelT . lift . lift

instance MonadRandom m => MonadRandom (CoinSelT utxo e m) where
  getRandomBytes = lift . getRandomBytes

coinSelLiftExcept :: Monad m => Except e a -> CoinSelT utxo e m a
coinSelLiftExcept (ExceptT (Identity (Left err))) = throwError err
coinSelLiftExcept (ExceptT (Identity (Right a)))  = return a

-- | Change errors
mapCoinSelErr :: Monad m
              => (e -> e')
              -> CoinSelT utxo e  m a
              -> CoinSelT utxo e' m a
mapCoinSelErr f act = wrapCoinSelT $ \st ->
    bimap f identity <$> unwrapCoinSelT act st

-- | Temporarily work with a different UTxO representation
mapCoinSelUtxo :: Monad m
               => (utxo' -> utxo)
               -> (utxo -> utxo')
               -> CoinSelT utxo  e m a
               -> CoinSelT utxo' e m a
mapCoinSelUtxo inj proj act = wrapCoinSelT $ \st ->
    bimap identity (bimap identity proj) <$> unwrapCoinSelT act (inj st)

-- | Unwrap the 'CoinSelT' stack
unwrapCoinSelT :: CoinSelT utxo e m a -> utxo -> m (Either e (a, utxo))
unwrapCoinSelT act = runExceptT . runStrictStateT (unCoinSelT act)

-- | Inverse of 'unwrapCoinSelT'
wrapCoinSelT :: Monad m
             => (utxo -> m (Either e (a, utxo))) -> CoinSelT utxo e m a
wrapCoinSelT f = CoinSelT $ strictStateT $ ExceptT . f

-- | Catch only certain errors
--
-- The signature of 'catchJust' is more general than the usual one, because
-- we make it clear /in the types/ that errors of a certain type /must/ be
-- caught.
catchJust :: Monad m
          => (e -> Either e1 e2)
          -> CoinSelT utxo e m a
          -> (e2 -> CoinSelT utxo e1 m a)
          -> CoinSelT utxo e1 m a
catchJust classify act handler = wrapCoinSelT $ \st -> do
    ma <- unwrapCoinSelT act st
    case ma of
      Right (a, st') -> return $ Right (a, st')
      Left  err      -> case classify err of
                          Left  e1 -> return $ Left e1
                          Right e2 -> unwrapCoinSelT (handler e2) st

{-------------------------------------------------------------------------------
  Errors
-------------------------------------------------------------------------------}

-- | This input selection request is unsatisfiable
--
-- These are errors we cannot recover from.
data CoinSelHardErr =
    -- | Payment to receiver insufficient to cover fee
    --
    -- We record the original output and the fee it needed to cover.
    --
    -- Only applicable using 'ReceiverPaysFees' regulation
    forall dom. CoinSelDom dom =>
      CoinSelHardErrOutputCannotCoverFee (Output dom) (Fee dom)

    -- | Attempt to pay into a redeem-only address
  | forall dom. CoinSelDom dom =>
      CoinSelHardErrOutputIsRedeemAddress (Output dom)

    -- | UTxO exhausted whilst trying to pick inputs to cover remaining fee
  | CoinSelHardErrCannotCoverFee

    -- | When trying to construct a transaction, the max number of allowed
    -- inputs was reached.
  | CoinSelHardErrMaxInputsReached Word64

    -- | UTxO exhausted during input selection
    --
    -- We record the balance of the UTxO as well as the size of the payment
    -- we tried to make.
    --
    -- See also 'CoinSelHardErrCannotCoverFee'
  | forall dom. CoinSelDom dom =>
      CoinSelHardErrUtxoExhausted (Value dom) (Value dom)

    -- | UTxO depleted using input selection
  | CoinSelHardErrUtxoDepleted

    -- | This wallet does not \"own\" the input address
  | forall dom. HasAddress dom =>
      CoinSelHardErrAddressNotOwned (Proxy dom) (Address dom)
      -- ^ We need a proxy here due to the fact that 'Address' is not
      -- injective.


instance Arbitrary CoinSelHardErr where
    arbitrary = pure CoinSelHardErrUtxoDepleted

-- | The input selection request failed
--
-- The algorithm failed to find a solution, but that doesn't necessarily mean
-- that there isn't one.
data CoinSelSoftErr = CoinSelSoftErr

-- | Union of the two kinds of input selection failures
data CoinSelErr =
    CoinSelErrHard CoinSelHardErr
  | CoinSelErrSoft CoinSelSoftErr

-- | Specialization of 'catchJust'
catchJustSoft :: Monad m
              => CoinSelT utxo CoinSelErr m a
              -> (CoinSelSoftErr -> CoinSelT utxo CoinSelHardErr m a)
              -> CoinSelT utxo CoinSelHardErr m a
catchJustSoft = catchJust $ \case CoinSelErrHard e' -> Left  e'
                                  CoinSelErrSoft e' -> Right e'

{-------------------------------------------------------------------------------
  Policy
-------------------------------------------------------------------------------}

-- | Coin selection policy returning result of type @a@
--
-- We are polymorphic in the result because instantiations of this framework
-- for different domains will return different kinds of results (signed
-- transaction, DSL transaction along with statistics, etc.)
type CoinSelPolicy utxo m a =
       NonEmpty (Output (Dom utxo))
    -> utxo  -- ^ Available UTxO
    -> m (Either CoinSelHardErr a)

{-------------------------------------------------------------------------------
  Coin selection result
-------------------------------------------------------------------------------}

-- | The result of coin selection for a single output
data CoinSelResult dom = CoinSelResult {
    -- | The output as it was requested
    coinSelRequest :: Output dom

    -- | The output as it should appear in the final tranasction
    --
    -- This may be different from the requested output if recipient pays fees.
  , coinSelOutput  :: Output dom

    -- | Change outputs (if any)
    --
    -- These are not outputs, to keep this agnostic to a choice of change addr
  , coinSelChange  :: [Value dom]

    -- | The UTxO entries that were used for this output
  , coinSelInputs  :: SelectedUtxo dom
  }

-- | Default 'CoinSelResult' where 'CoinSelOutput = coinSelRequest',
defCoinSelResult :: CoinSelDom dom
                 => Output dom -> SelectedUtxo dom -> CoinSelResult dom
defCoinSelResult goal selected = CoinSelResult {
      coinSelRequest = goal
    , coinSelOutput  = goal
    , coinSelChange  = [change]
    , coinSelInputs  = selected
    }
  where
    change = unsafeValueSub (selectedBalance selected) (outVal goal)

coinSelInputSize :: CoinSelResult dom -> Size dom
coinSelInputSize = selectedSize . coinSelInputs

coinSelInputSet :: CoinSelDom dom => CoinSelResult dom -> Set (Input dom)
coinSelInputSet = selectedInputs . coinSelInputs

coinSelOutputs :: CoinSelDom dom => CoinSelResult dom -> [Value dom]
coinSelOutputs cs = outVal (coinSelOutput cs) : coinSelChange cs

-- | Filter out any outputs from the coin selection that are smaller than
-- or equal to the dust threshold
coinSelRemoveDust :: CoinSelDom dom
                  => Value dom -> CoinSelResult dom -> CoinSelResult dom
coinSelRemoveDust dust cs = cs {
      coinSelChange = filter (> dust) (coinSelChange cs)
    }

-- | Do coin selection per goal
--
-- Coin selection per goal simplifies the algorithm, but is not without loss
-- of generality, as it cannot reuse a single UTxO entry for multiple goals.
--
-- NOTE: This is basically 'mapM', except that we thread the maximum nmber of
-- inputs through.
coinSelPerGoal :: forall m a dom. (Monad m, CoinSelDom dom)
               => (Word64 -> a   -> m (CoinSelResult dom))
               -> (Word64 -> [a] -> m [CoinSelResult dom])
coinSelPerGoal f = go []
  where
    go :: [CoinSelResult dom] -> Word64 -> [a] -> m [CoinSelResult dom]
    go acc _            []           = return acc
    go acc maxNumInputs (goal:goals) = do
        cs <- f maxNumInputs goal
        go (cs:acc) (maxNumInputs - sizeToWord (coinSelInputSize cs)) goals

{-------------------------------------------------------------------------------
  Helper data structture for defining coin selection policies
-------------------------------------------------------------------------------}

-- | Selection of UTxO entries
--
-- This is convenient for defining coin selection policies
--
-- Invariants:
--
-- > selectedSize    == sizeOfEntries  selectedEntries
-- > selectedBalance == unsafeValueSum selectedEntries
data SelectedUtxo dom = SelectedUtxo {
      selectedEntries :: ![UtxoEntry dom]
    , selectedBalance :: !(Value dom)
    , selectedSize    :: !(Size dom)
    }

emptySelection :: CoinSelDom dom => SelectedUtxo dom
emptySelection = SelectedUtxo {
      selectedEntries = []
    , selectedBalance = valueZero
    , selectedSize    = sizeZero
    }

-- | Select an entry and prepend it to `selectedEntries`
--
-- NOTE: We assume that these entries are selected from a well-formed UTxO,
-- and hence that this cannot overflow `selectedBalance`.
select :: CoinSelDom dom
       => UtxoEntry dom -> SelectedUtxo dom -> SelectedUtxo dom
select io SelectedUtxo{..} = SelectedUtxo {
      selectedEntries = io : selectedEntries
    , selectedBalance = unsafeValueAdd selectedBalance (utxoEntryVal io)
    , selectedSize    = sizeIncr io selectedSize
    }

selectedInputs :: CoinSelDom dom => SelectedUtxo dom -> Set (Input dom)
selectedInputs = Set.fromList . map utxoEntryInp . selectedEntries

{-------------------------------------------------------------------------------
  Generalization over UTxO representations
-------------------------------------------------------------------------------}

-- | Shape of standard UTxO (map)
class ( StandardDom (Dom utxo)
      , Coercible utxo (Map (Input (Dom utxo)) (Output (Dom utxo)))
      ) => StandardUtxo utxo where

-- | Abstraction over selecting entries from a UTxO
class CoinSelDom (Dom utxo) => PickFromUtxo utxo where
  -- | The domain that this UTxO representation lives in
  --
  -- NOTE: This type family is /not/ injective (a domain may have more than
  -- one possible UTxO representation).
  type Dom utxo :: *

  -- | Pick a random element from the UTxO
  pickRandom :: MonadRandom m => utxo -> m (Maybe (UtxoEntry (Dom utxo), utxo))

  -- | Return the N largest elements from the UTxO
  --
  -- This returns a list of pairs of entries and utxos; the second component
  -- in each pair is the utxo after the entries in the list /so far/ have
  -- been removed. The coin selection policy should decide for each entry
  -- in the list, in order, whether or not to use it, and use the UTxO
  -- associated with the last used entry as the new UTxO. Only this final
  -- UTxO should be forced.
  pickLargest :: Word64 -> utxo -> [(UtxoEntry (Dom utxo), utxo)]

  -- | Compute UTxO balance
  --
  -- This is used only for error reporting.
  utxoBalance :: utxo -> Value (Dom utxo)

  -- default definitions for maps

  default pickRandom :: forall m. (StandardUtxo utxo, MonadRandom m)
                     => utxo -> m (Maybe (UtxoEntry (Dom utxo), utxo))
  pickRandom = fmap (fmap (bimap identity coerce))
             . mapRandom
             . coerce

  default pickLargest :: StandardUtxo utxo
                      => Word64 -> utxo -> [(UtxoEntry (Dom utxo), utxo)]
  pickLargest n = fmap (bimap identity coerce)
                . nLargestFromMapBy outVal n
                . coerce

  default utxoBalance :: StandardUtxo utxo => utxo -> Value (Dom utxo)
  utxoBalance = unsafeValueSum . map outVal . Map.elems . coerce

{-------------------------------------------------------------------------------
  Helper functions for defining instances
-------------------------------------------------------------------------------}

-- | Pick a random element from a map
--
-- Returns 'Nothing' if the map is empty
mapRandom :: forall m k a. MonadRandom m
          => Map k a -> m (Maybe ((k, a), Map k a))
mapRandom m
  | Map.null m = return Nothing
  | otherwise  = withIx . fromIntegral <$>
                   generateBetween 0 (fromIntegral (Map.size m - 1))
  where
    withIx :: Int -> Maybe ((k, a), Map k a)
    withIx ix = Just (Map.elemAt ix m, Map.deleteAt ix m)

nLargestFromMapBy :: forall k a b. (Ord b, Ord k)
                  => (a -> b) -> Word64 -> Map k a -> [((k, a), Map k a)]
nLargestFromMapBy f n m =
    aux Set.empty $ nLargestFromListBy (f . snd) n (Map.toList m)
  where
    aux :: Set k -> [(k, a)] -> [((k, a), Map k a)]
    aux _       []             = []
    aux deleted ((k, a) : kas) = ((k, a), m `withoutKeys` deleted')
                               : aux deleted' kas
      where
        deleted' = Set.insert k deleted

-- | Return the @n@ largest elements of the list, from large to small.
--
-- @O(n)@
nLargestFromListBy :: forall a b. Ord b => (a -> b) -> Word64 -> [a] -> [a]
nLargestFromListBy f n = \xs ->
    -- If the map resulting from manipulating @xs@ is empty, we need to
    -- return straight away as otherwise the call to 'Map.findMin' later
    -- would fail.
    let (firstN, rest) = splitAt (fromIntegral n) xs
        acc            = Map.fromListWith (++) $ map (\a -> (f a, [a])) firstN
    in if Map.null acc then [] else go acc rest
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
    go' acc _ []    = concatMap snd $ Map.toDescList acc
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

-- | Proportionally divide the fee over each output
divvyFee :: forall dom a. CoinSelDom dom
          => (a -> Value dom) -> Fee dom -> [a] -> [(Fee dom, a)]
divvyFee _ _   [] = error "divvyFee: empty list"
divvyFee f fee as = map (\a -> (feeForOut a, a)) as
  where
    -- All outputs are selected from well-formed UTxO, so their sum cannot
    -- overflow
    totalOut :: Value dom
    totalOut = unsafeValueSum (map f as)

    -- The ratio will be between 0 and 1 so cannot overflow
    feeForOut :: a -> Fee dom
    feeForOut a =
        adjustFee (unsafeValueAdjust RoundUp (valueRatio (f a) totalOut)) fee


{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

instance Buildable CoinSelHardErr where
  build (CoinSelHardErrOutputCannotCoverFee out val) = bprint
    ( "CoinSelHardErrOutputCannotCoverFee"
    % "{ output: " % build
    % ", value:  " % build
    % "}"
    )
    out
    val
  build (CoinSelHardErrOutputIsRedeemAddress out) = bprint
    ( "CoinSelHardErrOutputIsRedeemAddress"
    % "{ output: " % build
    % "}"
    )
    out
  build (CoinSelHardErrMaxInputsReached inputs) = bprint
    ( "CoinSelHardErrMaxInputsReached"
    % "{ inputs: " % build
    % "}"
    )
    inputs
  build (CoinSelHardErrCannotCoverFee) = bprint
    ( "CoinSelHardErrCannotCoverFee" )
  build (CoinSelHardErrUtxoExhausted bal val) = bprint
    ( "CoinSelHardErrUtxoExhausted"
    % "{ balance: " % build
    % ", value:   " % build
    % "}"
    )
    bal
    val
  build (CoinSelHardErrUtxoDepleted) = bprint
    ( "CoinSelHardErrUtxoDepleted" )
  build (CoinSelHardErrAddressNotOwned _ addr) = bprint
    ( "CoinSelHardErrAddressNotOwned { address: " % build % " } ") addr

instance CoinSelDom dom => Buildable (Fee dom) where
  build = bprint build . getFee
