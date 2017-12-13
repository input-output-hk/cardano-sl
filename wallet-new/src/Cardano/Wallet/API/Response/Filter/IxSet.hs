module Cardano.Wallet.API.Response.Filter.IxSet where

import           Universum

import           Cardano.Wallet.API.Indices (Indexable', IsIndexOf', IxSet')
import qualified Cardano.Wallet.API.Request.Filter as F
import           Data.IxSet.Typed ((@<), (@=), (@>))

applyFilters :: Indexable' a
             => F.FilterOperations a
             -> IxSet' a
             -> IxSet' a
applyFilters F.NoFilters iset        = iset
applyFilters (F.FilterOp f fop) iset = applyFilters fop (applyFilter f iset)

applyFilter :: forall ix a.
               ( Indexable' a
               , IsIndexOf' a ix
               )
            => F.FilterOperation ix a
            -> IxSet' a
            -> IxSet' a
applyFilter fltr inputData =
    let byPredicate o i = case o of
            EQ -> inputData @= (i :: ix)
            LT -> inputData @< (i :: ix)
            GT -> inputData @> (i :: ix)
    in case fltr of
           F.FilterIdentity             -> inputData
           F.FilterByIndex idx          -> byPredicate EQ idx
           F.FilterByPredicate ordr idx -> byPredicate ordr idx
