{-- | Legacy and probably inefficient backend to filter data from the model. --}
module Cardano.Wallet.API.Response.Filter.Legacy (
    applyFilters
    ) where

import           Universum

import qualified Data.Set as Set

import           Cardano.Wallet.API.Indices (Indexable', IsIndexOf', ToIndex (..))
import qualified Cardano.Wallet.API.Request.Filter as F

-- | Applies all the input filters to the input list.
applyFilters :: (MonadPlus m, Indexable' a) => F.FilterOperations a -> m a -> m a
applyFilters F.NoFilters inputData        = inputData
applyFilters (F.FilterOp f fop) inputData = applyFilters fop (applyFilter f inputData)

-- | Applies a single 'FilterOperation' on the input data, producing filtered data as output.
applyFilter :: forall ix a m. (IsIndexOf' a ix, MonadPlus m, Indexable' a , ToIndex a ix)
            => F.FilterOperation ix a
            -> m a
            -> m a
applyFilter fltr inputData =
    let byPredicate o i = case o of
            F.Equal            -> filterData (\d -> accessIx d == i) inputData
            F.LesserThan       -> filterData (\d -> accessIx d <  i) inputData
            F.GreaterThan      -> filterData (\d -> accessIx d >  i) inputData
            F.LesserThanEqual  -> filterData (\d -> accessIx d <=  i) inputData
            F.GreaterThanEqual -> filterData (\d -> accessIx d >=  i) inputData
    in case fltr of
           F.FilterByIndex idx          -> byPredicate F.Equal idx
           F.FilterByPredicate ordr idx -> byPredicate ordr idx
           F.FilterByRange from to      -> filterData (\d -> accessIx d >= from && accessIx d <= to) inputData
           F.FilterIn ixs               ->
               let ixs' = Set.fromList ixs
                in filterData (\d -> accessIx d `Set.member` ixs') inputData

-- A simple and unoptimised generic 'filter' function running in 'MonadPlus'
-- See: http://conal.net/blog/posts/a-handy-generalized-filter
filterData :: MonadPlus m => (a -> Bool) -> m a -> m a
filterData p m = joinMaybes (liftM f m)
 where
   f a | p a       = Just a
       | otherwise = Nothing

   joinMaybes :: MonadPlus m => m (Maybe a) -> m a
   joinMaybes = (>>= maybe mzero return)
