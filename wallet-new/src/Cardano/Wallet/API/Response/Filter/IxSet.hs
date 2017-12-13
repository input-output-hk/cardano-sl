module Cardano.Wallet.API.Response.Filter.IxSet where

import           Universum

import qualified Cardano.Wallet.API.Request.Filter as F
import           Cardano.Wallet.API.V1.Types
import           Data.IxSet.Typed (Indexable (..), IsIndexOf, IxSet, ixFun, ixList, (@<), (@=),
                                   (@>))

type Wallets = IxSet F.WalletIxs Wallet

instance Indexable F.WalletIxs Wallet where
  indices = ixList (ixFun (\Wallet{..} -> [walId]))
                   (ixFun (\Wallet{..} -> [walBalance]))

{-
applyFilters :: (Indexable ixs a)
             => F.FilterOperations ixs a
             -> IxSet ixs a
             -> IxSet ixs a
applyFilters F.FNil iset        = iset
applyFilters (f F.::: fop) iset = applyFilters fop (applyFilter f iset)
-}

applyFilter :: forall ix ixs a.
               ( F.IsIndexOf a ix
               , Indexable ixs a
               , IsIndexOf ix ixs
               )
            => F.FilterOperation ix a
            -> IxSet ixs a
            -> IxSet ixs a
applyFilter fltr inputData =
    let byPredicate o i = case o of
            EQ -> inputData @= (i :: ix)
            LT -> inputData @< (i :: ix)
            GT -> inputData @> (i :: ix)
    in case fltr of
           F.FilterNoOp                 -> inputData
           F.FilterByIndex idx          -> byPredicate EQ idx
           F.FilterByPredicate ordr idx -> byPredicate ordr idx

-- filter :: [FilterOperation a] -> Ix.IxSet ixs a
