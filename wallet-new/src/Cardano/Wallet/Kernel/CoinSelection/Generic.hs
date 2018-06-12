{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilyDependencies     #-}

module Cardano.Wallet.Kernel.CoinSelection.Generic (
    -- * Domain
    CoinSelDom(..)
  , UtxoEntry
  , unsafeValueAdd
  , unsafeValueSub
    -- * Monad
  , CoinSelT -- opaque
  , mapCoinSelErr
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
  , coinSelCountInputs
  , coinSelRemoveDust
  , coinSelPerGoal
    -- * Generalization over UTxO representations
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
    -- * Convenience re-exports
  , MonadError(..)
  , MonadRandom
  , withoutKeys
  ) where

import           Universum

import           Control.Monad.Except (MonadError (..))
import           Crypto.Number.Generate (generateBetween)
import           Crypto.Random (MonadRandom (..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import           Cardano.Wallet.Kernel.Util.StrictStateT

{-------------------------------------------------------------------------------
  Abstract domain
-------------------------------------------------------------------------------}

class ( Ord (Value dom)
      , Ord (Input dom)
        -- Buildable instances to aid debugging
      , Buildable (Input  dom)
      , Buildable (Output dom)
      , Buildable (Value  dom)
      ) => CoinSelDom dom where
  type Input   dom = i | i -> dom
  type Output  dom = o | o -> dom
  type Value   dom = v | v -> dom

  outVal :: Output dom -> Value dom

  valueZero :: Value dom
  valueAdd  :: Value dom -> Value dom -> Maybe (Value dom)
  valueSub  :: Value dom -> Value dom -> Maybe (Value dom)
  valueMult :: Value dom -> Int -> Maybe (Value dom)

  -- | Absolute distance between two values
  --
  -- NOTE: This should be a total function!
  valueDist :: Value dom -> Value dom -> Value dom

type UtxoEntry dom = (Input dom, Output dom)

unsafeValueAdd :: CoinSelDom dom => Value dom -> Value dom -> Value dom
unsafeValueAdd x y = fromMaybe (error "unsafeValueAdd: overflow") $
    valueAdd x y

unsafeValueSub :: CoinSelDom dom => Value dom -> Value dom -> Value dom
unsafeValueSub x y = fromMaybe (error "unsafeValueSub: underflow") $
    valueSub x y

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

-- | Change errors
mapCoinSelErr :: Monad m
              => (e -> e')
              -> CoinSelT utxo e  m a
              -> CoinSelT utxo e' m a
mapCoinSelErr f act = wrapCoinSelT $ \st ->
    bimap f identity <$> unwrapCoinSelT act st

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
data CoinSelHardErr = CoinSelHardErr

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
catchJustSoft = catchJust $ \e -> case e of
                                    CoinSelErrHard e' -> Left  e'
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
       [Output (Dom utxo)]
    -> utxo  -- ^ Available UTxO
    -> m (Either CoinSelHardErr (a, utxo))

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

coinSelCountInputs :: CoinSelResult dom -> Int
coinSelCountInputs = selectedCount . coinSelInputs

coinSelInputSet :: CoinSelDom dom => CoinSelResult dom -> Set (Input dom)
coinSelInputSet = selectedInputs . coinSelInputs

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
coinSelPerGoal :: forall m a dom. Monad m
               => (Int -> a   -> m (CoinSelResult dom))
               -> (Int -> [a] -> m [CoinSelResult dom])
coinSelPerGoal f = go []
  where
    go :: [CoinSelResult dom] -> Int -> [a] -> m [CoinSelResult dom]
    go acc _            []           = return acc
    go acc maxNumInputs (goal:goals) = do
        cs <- f maxNumInputs goal
        go (cs:acc) (maxNumInputs - coinSelCountInputs cs) goals

{-------------------------------------------------------------------------------
  Helper data structture for defining coin selection policies
-------------------------------------------------------------------------------}

-- | Selection of UTxO entries
--
-- This is convenient for defining coin selection policies
--
-- Invariants:
--
-- > selectedCount   == length selectedEntries
-- > selectedBalance == foldr unsafeValueAdd valueZero selectedEntries
data SelectedUtxo dom = SelectedUtxo {
      selectedEntries :: ![UtxoEntry dom]
    , selectedBalance :: !(Value dom)
    , selectedCount   :: !Int
    }

emptySelection :: CoinSelDom dom => SelectedUtxo dom
emptySelection = SelectedUtxo {
      selectedEntries = []
    , selectedBalance = valueZero
    , selectedCount   = 0
    }

-- | Select an entry and prepend it to `selectedEntries`
--
-- NOTE: We assume that these entries are selected from a well-formed UTxO,
-- and hence that this cannot overflow `selectedBalance`.
select :: CoinSelDom dom
       => UtxoEntry dom -> SelectedUtxo dom -> SelectedUtxo dom
select (i, o) SelectedUtxo{..} = SelectedUtxo {
      selectedEntries = (i, o) : selectedEntries
    , selectedBalance = unsafeValueAdd selectedBalance (outVal o)
    , selectedCount   = selectedCount + 1
    }

selectedInputs :: CoinSelDom dom => SelectedUtxo dom -> Set (Input dom)
selectedInputs = Set.fromList . map fst . selectedEntries

{-------------------------------------------------------------------------------
  Generalization over UTxO representations
-------------------------------------------------------------------------------}

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
  pickLargest :: Int -> utxo -> [(UtxoEntry (Dom utxo), utxo)]

  -- default definitions for maps

  default pickRandom :: forall m.
                        ( utxo ~ Map (Input (Dom utxo)) (Output (Dom utxo))
                        , MonadRandom m
                        )
                     => utxo -> m (Maybe (UtxoEntry (Dom utxo), utxo))
  pickRandom = mapRandom

  default pickLargest :: utxo ~ Map (Input (Dom utxo)) (Output (Dom utxo))
                      => Int -> utxo -> [(UtxoEntry (Dom utxo), utxo)]
  pickLargest = nLargestFromMapBy outVal

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
                  => (a -> b) -> Int -> Map k a -> [((k, a), Map k a)]
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
nLargestFromListBy :: forall a b. Ord b => (a -> b) -> Int -> [a] -> [a]
nLargestFromListBy f n = \xs ->
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

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

withoutKeys :: Ord k => Map k a -> Set k -> Map k a
m `withoutKeys` s = m `Map.difference` Map.fromSet (const ()) s
