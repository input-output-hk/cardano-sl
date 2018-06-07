-- | Sorted UTxO
--
-- Intended for qualified import.
module InputSelection.SortedUtxo (
    SortedUtxo -- opaque
  , maxView
  ) where

import           Universum hiding (empty)

import qualified Data.Map.Strict as Map

import           InputSelection.Policy
import           UTxO.DSL (Utxo)
import qualified UTxO.DSL as DSL

-- | UTxO sorted by value
--
-- Invariant: none of the nested 'Utxo' should be empty.
newtype SortedUtxo h a = SortedUtxo {
      sorted  :: Map Value (Utxo h a)
    }

instance IsUtxo SortedUtxo where
  utxoEmpty        = empty
  utxoUnion        = union
  utxoSize         = size
  utxoBalance      = balance
  utxoOutputs      = outputs
  utxoRemoveInputs = removeInputs

{-------------------------------------------------------------------------------
  Basic operations
-------------------------------------------------------------------------------}

empty :: SortedUtxo h a
empty = SortedUtxo Map.empty

-- | Add in entries from a "normal" UTxO
union :: Hash h a => Utxo h a -> SortedUtxo h a -> SortedUtxo h a
union = union' . sortedUtxo

-- | Number of entries in the UTxO
size :: SortedUtxo h a -> Int
size = sum . map utxoSize . Map.elems . sorted

-- | Total balance
balance :: SortedUtxo h a -> Value
balance = sum . map utxoBalance . Map.elems . sorted

-- | List of all output values
--
-- The length of this list should be equal to 'utxoSize'
outputs :: Hash h a => SortedUtxo h a -> [Value]
outputs = map (outVal . snd) . concatMap DSL.utxoToList .  Map.elems . sorted

-- | Remove inputs from the domain
--
-- We take the inputs as a UTxO so that we know what their balance is.
removeInputs :: Hash h a => Utxo h a -> SortedUtxo h a -> SortedUtxo h a
removeInputs toRemove u = difference u (sortedUtxo toRemove)

{-------------------------------------------------------------------------------
  Specialized operations
-------------------------------------------------------------------------------}

maxView :: forall h a. Hash h a
        => SortedUtxo h a -> Maybe ((Input h a, Output a), SortedUtxo h a)
maxView (SortedUtxo u) = do
    ((val, elems), u') <- Map.maxViewWithKey u
    case DSL.utxoToList elems of
      []          -> error "SortedUtxo: maxView: invariant violation"
      io : []     -> Just (io, SortedUtxo $ u')
      io : elems' -> Just (io, SortedUtxo $ Map.insert val (DSL.utxoFromList elems') u')

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

sortedUtxo :: forall h a. Hash h a => Utxo h a -> SortedUtxo h a
sortedUtxo = go empty . DSL.utxoToList
  where
    go :: SortedUtxo h a -> [(Input h a, Output a)] -> SortedUtxo h a
    go acc []           = acc
    go acc ((i, o):ios) = go acc' ios
      where
        acc' :: SortedUtxo h a
        acc' = SortedUtxo $ Map.alter insert (outVal o) (sorted acc)

        insert :: Maybe (Utxo h a) -> Maybe (Utxo h a)
        insert Nothing    = Just $ DSL.utxoSingleton i o
        insert (Just old) = Just $ DSL.utxoInsert (i, o) old

union' :: Hash h a => SortedUtxo h a -> SortedUtxo h a -> SortedUtxo h a
union' (SortedUtxo u) (SortedUtxo u') = SortedUtxo $
    Map.unionWith DSL.utxoUnion u u'

-- | Return elements of the first UTxO not existing in the second
difference :: forall h a. Hash h a
           => SortedUtxo h a -> SortedUtxo h a -> SortedUtxo h a
difference (SortedUtxo u) (SortedUtxo u') = SortedUtxo $
    Map.differenceWith aux u u'
  where
    aux :: Utxo h a -> Utxo h a -> Maybe (Utxo h a)
    aux v v' = let v'' = DSL.utxoRemoveInputs (DSL.utxoDomain v') v
               in if DSL.utxoNull v'' then Nothing
                                      else Just v''
