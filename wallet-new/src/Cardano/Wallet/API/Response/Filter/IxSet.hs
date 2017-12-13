module Cardano.Wallet.API.Response.Filter.IxSet where

import           Universum

import qualified Cardano.Wallet.API.Request.Filter as F
import           Cardano.Wallet.API.V1.Types
import           Data.IxSet.Typed (Indexable (..), ixFun, ixList, (@<), (@=), (@>))

instance Indexable F.WalletIxs Wallet where
  indices = ixList (ixFun (\Wallet{..} -> [walId]))
                   (ixFun (\Wallet{..} -> [walBalance]))

instance Indexable F.TransactionIxs Transaction where
  indices = ixList (ixFun (\Transaction{..} -> [txId]))

applyFilters :: F.Indexable' a
             => F.FilterOperations a
             -> F.IxSet' a
             -> F.IxSet' a
applyFilters F.NoFilters iset        = iset
applyFilters (F.FilterOp f fop) iset = applyFilters fop (applyFilter f iset)

applyFilter :: forall ix a.
               ( F.Indexable' a
               , F.IsIndexOf' a ix
               )
            => F.FilterOperation ix a
            -> F.IxSet' a
            -> F.IxSet' a
applyFilter fltr inputData =
    let byPredicate o i = case o of
            EQ -> inputData @= (i :: ix)
            LT -> inputData @< (i :: ix)
            GT -> inputData @> (i :: ix)
    in case fltr of
           F.FilterIdentity             -> inputData
           F.FilterByIndex idx          -> byPredicate EQ idx
           F.FilterByPredicate ordr idx -> byPredicate ordr idx
