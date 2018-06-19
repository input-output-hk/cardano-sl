-- | Sorted UTxO
--
-- Intended for qualified import.
module InputSelection.SortedUtxo (
    SortedUtxo -- opaque
    -- * Basic operations
  , empty
  , union
  , size
  , balance
  , outputs
  , removeInputs
    -- * Specialized operations
  , maxView
    -- * Conversion
  , fromMap
  , toMap
  ) where

import           Universum hiding (empty, sort)

import qualified Data.Map.Strict as Map
import qualified Data.Text.Buildable
import           Formatting (bprint)
import           Serokell.Util (mapJson)

import           Cardano.Wallet.Kernel.CoinSelection.Generic

{-------------------------------------------------------------------------------
  Sorted UTxO
-------------------------------------------------------------------------------}

-- | UTxO sorted by value
--
-- This is useful when running simulations on largest-first as the main
-- algorithm, so that we don't have to sort at each step.
--
-- Invariant: none of the nested 'Utxo' should be empty.
newtype SortedUtxo dom = SortedUtxo {
      sorted :: Map (Value dom) (Map (Input dom) (Output dom))
    }

instance CoinSelDom dom => PickFromUtxo (SortedUtxo dom) where
  type Dom (SortedUtxo dom) = dom

  pickRandom  = error "pickRandom not implemented for SortedUtxo"
  pickLargest = nLargest
  utxoBalance = balance

{-------------------------------------------------------------------------------
  Basic operations
-------------------------------------------------------------------------------}

empty :: SortedUtxo dom
empty = SortedUtxo Map.empty

-- | Number of entries in the UTxO
size :: SortedUtxo dom -> Int
size = sum . map Map.size . Map.elems . sorted

-- | Total balance
balance :: CoinSelDom dom => SortedUtxo dom -> Value dom
balance = foldr unsafeValueAdd valueZero . outputs

-- | List of all output values
--
-- The length of this list should be equal to 'size'
outputs :: CoinSelDom dom => SortedUtxo dom -> [Value dom]
outputs = map outVal . concatMap Map.elems . sorted

-- | Remove inputs from the domain
--
-- We take the inputs as a UTxO so that we know what their balance is.
removeInputs :: forall dom. CoinSelDom dom
             => Set (Input dom) -> SortedUtxo dom -> SortedUtxo dom
removeInputs toRemove (SortedUtxo u) = SortedUtxo $
    Map.mapMaybe aux u
  where
    aux :: Map (Input dom) (Output dom) -> Maybe (Map (Input dom) (Output dom))
    aux m = do let m' = m `withoutKeys` toRemove
               guard $ not (Map.null m')
               return m'

union :: CoinSelDom dom
      => SortedUtxo dom -> SortedUtxo dom -> SortedUtxo dom
union (SortedUtxo u) (SortedUtxo u') = SortedUtxo $
    Map.unionWith Map.union u u'

{-------------------------------------------------------------------------------
  Specialized operations
-------------------------------------------------------------------------------}

-- | Repeated application of 'maxView'
nLargest :: CoinSelDom dom
         => Int -> SortedUtxo dom -> [((Input dom, Output dom), SortedUtxo dom)]
nLargest 0 _ = []
nLargest n u = case maxView u of
                 Nothing      -> []
                 Just (x, u') -> (x, u') : nLargest (n - 1) u'

-- | Select largest element from the UTxO
--
-- @O(1)@
maxView :: CoinSelDom dom
        => SortedUtxo dom -> Maybe ((Input dom, Output dom), SortedUtxo dom)
maxView (SortedUtxo u) = do
    ((val, elems), u') <- Map.maxViewWithKey u
    case pickOne elems of
      (io, Nothing)     -> Just (io, SortedUtxo $ u')
      (io, Just elems') -> Just (io, SortedUtxo $ Map.insert val elems' u')
  where
    pickOne :: Ord k => Map k a -> ((k, a), Maybe (Map k a))
    pickOne m = case Map.toList m of
                  []   -> error "SortedUtxo: maxView: invariant violation"
                  [e]  -> (e, Nothing)
                  e:es -> (e, Just (Map.fromList es))

{-------------------------------------------------------------------------------
  Conversion
-------------------------------------------------------------------------------}

fromMap :: forall dom. CoinSelDom dom
        => Map (Input dom) (Output dom) -> SortedUtxo dom
fromMap = go empty . Map.toList
  where
    go :: SortedUtxo dom -> [(Input dom, Output dom)] -> SortedUtxo dom
    go acc []           = acc
    go acc ((i, o):ios) = go acc' ios
      where
        acc' :: SortedUtxo dom
        acc' = SortedUtxo $ Map.alter insert (outVal o) (sorted acc)

        insert :: Maybe (Map (Input dom) (Output (dom)))
               -> Maybe (Map (Input dom) (Output (dom)))
        insert Nothing    = Just $ Map.singleton i o
        insert (Just old) = Just $ Map.insert i o old

toMap :: CoinSelDom dom => SortedUtxo dom -> Map (Input dom) (Output dom)
toMap = Map.unions . Map.elems . sorted

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

instance CoinSelDom dom => Buildable (SortedUtxo dom) where
  build = bprint mapJson . toMap
