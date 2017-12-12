module Cardano.Wallet.API.Response.Filter.Legacy where

import           Universum

import qualified Cardano.Wallet.API.Request.Filter as F
import           Cardano.Wallet.API.V1.Types
import           Data.Constraint
import           Data.IxSet.Typed (Indexable (..), IsIndexOf, IxSet, ixFun, ixList, (@<), (@=),
                                   (@>))

type WalletIxs = '[WalletId]
type Wallets = IxSet WalletIxs Wallet

instance Indexable WalletIxs Wallet where
  indices = ixList (ixFun getWalletIndices)

getWalletIndices :: Wallet -> [WalletId]
getWalletIndices Wallet{..} = [walId]


applyFilter :: forall a ix ixs. ( F.IsIndexOf a ix
               , Indexable ixs a
               , IsIndexOf ix ixs
               )
            => IxSet ixs a
            -> F.FilterOperation ix a
            -> IxSet ixs a
applyFilter inputData fltr =
    let byPredicate o i = case o of
            EQ -> inputData @= (i :: ix)
            LT -> inputData @< (i :: ix)
            GT -> inputData @> (i :: ix)
    in case fltr of
           F.FilterNoOp                 -> inputData
           F.FilterByIndex idx          -> byPredicate EQ idx
           F.FilterByPredicate ordr idx -> byPredicate ordr idx

-- filter :: [FilterOperation a] -> Ix.IxSet ixs a
