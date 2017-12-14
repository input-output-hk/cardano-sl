module Cardano.Wallet.API.Response.Sort.IxSet where

import           Universum hiding (toList)

import           Cardano.Wallet.API.Indices (Indexable', IsIndexOf', IxSet')
import qualified Cardano.Wallet.API.Request.Sort as S
import           Data.IxSet.Typed (toAscList, toDescList, toList)

-- | Sort the data, stopping at the first encountered sort operation. This is because
-- sorting on multiple fields doesn't make sense with the current data model.
sortData :: Indexable' a => S.SortOperations a -> IxSet' a -> [a]
sortData S.NoSorts iset        = toList iset
sortData (S.SortOp s sop) iset =
    case s of
        -- If we don't need to sort the data, we skip this SortOperation.
        S.SortIdentity -> sortData sop iset
        _              -> applySort s iset

-- | Applies a single 'SortOperation' on the input 'IxSet'', producing a list of results afterwards.
applySort :: forall ix a. ( Indexable' a , IsIndexOf' a ix) => S.SortOperation ix a -> IxSet' a -> [a]
applySort sorts inputData = case sorts of
    S.SortIdentity                   -> toList inputData
    S.SortByIndex S.SortAscending _  -> toAscList (Proxy @ ix) inputData
    S.SortByIndex S.SortDescending _ -> toDescList (Proxy @ ix) inputData
