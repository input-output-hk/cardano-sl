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
  , fromUtxo
  , toUtxo
  ) where

import           Universum hiding (empty)

import           Data.Coerce (coerce)
import qualified Data.Map.Strict as Map
import           Formatting (bprint)
import qualified Formatting.Buildable
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

instance StandardDom dom => PickFromUtxo (SortedUtxo dom) where
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
    aux :: Map (Input dom) o -> Maybe (Map (Input dom) o)
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
nLargest :: StandardDom dom
         => Word64 -> SortedUtxo dom -> [(UtxoEntry dom, SortedUtxo dom)]
nLargest 0 _ = []
nLargest n u = case maxView u of
                 Nothing      -> []
                 Just (x, u') -> (x, u') : nLargest (n - 1) u'

-- | Select largest element from the UTxO
--
-- @O(1)@
maxView :: StandardDom dom
        => SortedUtxo dom -> Maybe (UtxoEntry dom, SortedUtxo dom)
maxView (SortedUtxo u) = do
    ((val, elements), u') <- Map.maxViewWithKey u
    case pickOne elements of
      (io, Nothing)     -> Just (io, SortedUtxo $ u')
      (io, Just elems') -> Just (io, SortedUtxo $ Map.insert val elems' u')
  where
    pickOne :: Ord k => Map k a -> ((k, a), Maybe (Map k a))
    pickOne m = case Map.toList m of
                  []     -> error "SortedUtxo: maxView: invariant violation"
                  [e]    -> (e, Nothing)
                  (e:es) -> (e, Just (Map.fromList es))

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

        insert :: Maybe (Map (Input dom) (Output dom))
               -> Maybe (Map (Input dom) (Output dom))
        insert Nothing    = Just $ Map.singleton i o
        insert (Just old) = Just $ Map.insert i o old

toMap :: CoinSelDom dom => SortedUtxo dom -> Map (Input dom) (Output dom)
toMap = Map.unions . Map.elems . sorted

fromUtxo :: forall utxo. StandardUtxo utxo => utxo -> SortedUtxo (Dom utxo)
fromUtxo = fromMap . coerce

toUtxo :: StandardUtxo utxo => SortedUtxo (Dom utxo) -> utxo
toUtxo = coerce . toMap

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

instance CoinSelDom dom => Buildable (SortedUtxo dom) where
  build = bprint mapJson . toMap
