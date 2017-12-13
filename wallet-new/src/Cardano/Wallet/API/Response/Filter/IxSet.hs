module Cardano.Wallet.API.Response.Filter.IxSet where

import           Universum

import qualified Cardano.Wallet.API.Request.Filter as F
import           Cardano.Wallet.API.V1.Types
import           Data.IxSet.Typed (Indexable (..), IsIndexOf, IxSet, ixFun, ixList, (@<), (@=),
                                   (@>))

instance Indexable F.WalletIxs Wallet where
  indices = ixList (ixFun (\Wallet{..} -> [walId]))
                   (ixFun (\Wallet{..} -> [walBalance]))

instance Indexable F.TransactionIxs Transaction where
  indices = ixList (ixFun (\Transaction{..} -> [txId]))

{-
applyFilters :: (Indexable ixs a)
             => Proxy ixs
             -> F.FilterOperations ixs a
             -> IxSet ixs a
             -> IxSet ixs a
applyFilters Proxy F.NoFilters iset     = iset
applyFilters p@Proxy (f F.::: fop) iset =
    case applyFilter p f iset of
        (x :: IxSet ixs a) -> applyFilters p fop x
-}

{-
class FilterApplication (ixs :: [*]) a where
    applyFilters :: Proxy ixs -> F.FilterOperations ixs a -> IxSet ixs a -> IxSet ixs a

instance FilterApplication '[] a where
    applyFilters _ _ x = x

instance (IsIndexOf ix ixs, Indexable ixs a, FilterApplication ixs a) => FilterApplication (ix ': ixs) a where
    applyFilters (f F.::: fop) x = applyFilters fop (applyFilter f x)
-}

applyFilter :: forall ix ixs a.
               ( Indexable ixs a
               , IsIndexOf ix ixs
               )
            => Proxy ixs
            -> F.FilterOperation ix a
            -> IxSet ixs a
            -> IxSet ixs a
applyFilter Proxy fltr inputData =
    let byPredicate o i = case o of
            EQ -> inputData @= (i :: ix)
            LT -> inputData @< (i :: ix)
            GT -> inputData @> (i :: ix)
    in case fltr of
           F.FilterIdentity             -> inputData
           F.FilterByIndex idx          -> byPredicate EQ idx
           F.FilterByPredicate ordr idx -> byPredicate ordr idx
