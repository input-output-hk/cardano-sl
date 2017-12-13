module Cardano.Wallet.API.Response.Sort.IxSet where

import           Universum hiding (toList)

import           Cardano.Wallet.API.Indices (Indexable', IsIndexOf', IxSet')
import qualified Cardano.Wallet.API.Request.Sort as S
import           Data.IxSet.Typed (toAscList, toDescList, toList)

-- | Sort the data, stopping at the first encountered sort operation. This is because
-- sorting on multiple fields doesn't make sense in the current architecture.
sortData :: Indexable' a
         => S.SortOperations a
         -> IxSet' a
         -> [a]
sortData S.NoSorts iset        = toList iset
sortData (S.SortOp s sop) iset =
    case s of
        S.SortIdentity -> sortData sop iset
        _              -> applySort s iset

applySort :: forall ix a.
               ( Indexable' a
               , IsIndexOf' a ix
               )
            => S.SortOperation ix a
            -> IxSet' a
            -> [a]
applySort sorts inputData = case sorts of
    S.SortIdentity                   -> toList inputData
    S.SortByIndex S.SortAscending _  -> toAscList (Proxy @ ix) inputData
    S.SortByIndex S.SortDescending _ -> toDescList (Proxy @ ix) inputData
