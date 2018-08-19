module Cardano.Wallet.API.Response.Sort.IxSet where

import           Cardano.Wallet.API.Indices (IsIndexOf, IxSet)
import qualified Cardano.Wallet.API.Request.Sort as S
import           Cardano.Wallet.Kernel.DB.Util.IxSet (toAscList, toDescList,
                     toList)

-- | Sort the data, stopping at the first encountered sort operation. This is because
-- sorting on multiple fields doesn't make sense with the current data model.
sortData :: S.SortOperations a -> IxSet a -> [a]
sortData S.NoSorts iset      = toList iset
sortData (S.SortOp s _) iset = applySort s iset

-- | Applies a single 'SortOperation' on the input 'IxSet'', producing a list of results afterwards.
applySort :: forall ix a. (IsIndexOf ix a) => S.SortOperation ix a -> IxSet a -> [a]
applySort sorts inputData = case sorts of
    S.SortByIndex S.SortAscending  p -> toAscList  p inputData
    S.SortByIndex S.SortDescending p -> toDescList p inputData
