{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE RankNTypes                #-}
module Cardano.Wallet.API.Request.Filter where

import qualified Prelude
import           Universum

import qualified Cardano.Wallet.API.Request.Parameters as Param
import           Cardano.Wallet.API.V1.Types
import           Cardano.Wallet.TypeLits (KnownSymbols, symbolVals)
import qualified Data.List as List
import qualified Data.Text as T
import           Data.Typeable (Typeable)
import qualified Generics.SOP as SOP
import           GHC.TypeLits (Symbol)

import           Cardano.Wallet.API.Indices
import           Network.HTTP.Types (parseQueryText)
import           Network.Wai (Request, rawQueryString)
import           Servant
import           Servant.Server.Internal

--
-- Filtering data
--

-- | A "bag" of filter operations, where the index constraint are captured in
-- the inner closure of 'FilterOp'.
data FilterOperations a where
    NoFilters  :: FilterOperations a
    FilterOp   :: (Indexable' a, IsIndexOf' a ix, ToIndex a ix)
               => FilterOperation ix a
               -> FilterOperations a
               -> FilterOperations a

instance Show (FilterOperations a) where
    show = show . flattenOperations

-- | Handy helper function to show opaque 'FilterOperation'(s), mostly for
-- debug purposes.
flattenOperations :: FilterOperations a -> [String]
flattenOperations NoFilters       = mempty
flattenOperations (FilterOp f fs) = show f : flattenOperations fs

-- A custom ordering for a 'FilterOperation'. Conceptually theh same as 'Ordering' but with the ">=" and "<="
-- variants.
data FilterOrdering =
      Equal
    | GreaterThan
    | GreaterThanEqual
    | LesserThan
    | LesserThanEqual
    deriving (Show, Eq)

-- A filter operation on the data model
data FilterOperation ix a =
      FilterByIndex ix
    -- ^ Filter by index (e.g. equal to)
    | FilterByPredicate FilterOrdering ix
    -- ^ Filter by predicate (e.g. lesser than, greater than, etc.)
    | FilterByRange ix ix
    -- ^ Filter by range, in the form [from,to]
    | FilterIdentity
    -- ^ Do not alter the resource.

instance Show (FilterOperation ix a) where
    show (FilterByIndex _)            = "FilterByIndex"
    show (FilterByPredicate theOrd _) = "FilterByPredicate[" <> show theOrd <> "]"
    show (FilterByRange _ _)          = "FilterByRange"
    show FilterIdentity               = "FilterIdentity"

-- | Represents a filter operation on the data model.
-- Examples:
--   *    `wallet_id=DEADBEEF`.
--   *    `balance=GT[10]`
--   *    `balance=RANGE[0,10]`
data FilterBy (sym :: [Symbol]) (r :: *) deriving Typeable

-- | This is a slighly boilerplat-y type family which maps symbols to
-- indices, so that we can later on reify them into a list of valid indices.
type family FilterParams (syms :: [Symbol]) (r :: *) :: [*] where
    FilterParams '[Param.WalletId, Param.Balance] Wallet = IndicesOf Wallet
    FilterParams '[Param.Id, Param.CreatedAt] Transaction = IndicesOf Transaction

class ToFilterOperations (ixs :: [*]) a where
  toFilterOperations :: Request -> [Text] -> proxy ixs -> FilterOperations a

instance Indexable' a => ToFilterOperations ('[]) a where
  toFilterOperations _ _ _ = NoFilters

instance ( Indexable' a
         , IsIndexOf' a ix
         , ToIndex a ix
         , ToFilterOperations ixs a
         )
         => ToFilterOperations (ix ': ixs) a where
  toFilterOperations req [] _     =
      let newOp = FilterIdentity
      in FilterOp (newOp :: FilterOperation ix a) (toFilterOperations req [] (Proxy :: Proxy ixs))
  toFilterOperations req (x:xs) _ =
      case List.lookup x (parseQueryText $ rawQueryString req) of
          Nothing       ->
              let newOp = FilterIdentity
              in FilterOp (newOp :: FilterOperation ix a) (toFilterOperations req xs (Proxy @ ixs))
          Just Nothing  ->
              let newOp = FilterIdentity
              in FilterOp (newOp :: FilterOperation ix a) (toFilterOperations req xs (Proxy @ ixs))
          Just (Just v) ->
              case parseFilterOperation (Proxy @a) (Proxy @ix) v of
                  Left _      ->
                      let newOp = FilterIdentity
                      in FilterOp (newOp :: FilterOperation ix a) (toFilterOperations req xs (Proxy @ ixs))
                  Right newOp -> newOp `FilterOp` toFilterOperations req xs (Proxy @ ixs)

instance ( HasServer subApi ctx
         , FilterParams syms res ~ ixs
         , KnownSymbols syms
         , ToFilterOperations ixs res
         , SOP.All (ToIndex res) ixs
         ) => HasServer (FilterBy syms res :> subApi) ctx where

    type ServerT (FilterBy syms res :> subApi) m = FilterOperations res -> ServerT subApi m
    hoistServerWithContext _ ct hoist' s = hoistServerWithContext (Proxy @subApi) ct hoist' . s

    route Proxy context subserver =
        let allParams = map toText $ symbolVals (Proxy @syms)
            delayed = addParameterCheck subserver . withRequest $ \req ->
                          return $ toFilterOperations req allParams (Proxy @ixs)

        in route (Proxy :: Proxy subApi) context delayed

parseFilterParams :: forall a ixs. (
                     SOP.All (ToIndex a) ixs
                  ,  ToFilterOperations ixs a
                  )
                  => Request
                  -> [Text]
                  -> Proxy ixs
                  -> DelayedIO (FilterOperations a)
parseFilterParams req params p = return $ toFilterOperations req params p

-- | Parse the filter operations, failing silently if the query is malformed.
-- TODO(adinapoli): we need to improve error handling (and the parsers, for
-- what is worth).
parseFilterOperation :: forall a ix. ToIndex a ix
                     => Proxy a
                     -> Proxy ix
                     -> Text
                     -> Either Text (FilterOperation ix a)
parseFilterOperation p Proxy txt = case parsePredicateQuery <|> parseIndexQuery of
    Nothing -> Left "Not a valid filter."
    Just f  -> Right f
  where
    parsePredicateQuery :: Maybe (FilterOperation ix a)
    parsePredicateQuery =
        let (predicate, rest1) = T.breakOn "[" txt
            (ixTxt, closing)   = T.breakOn "]" (T.drop 1 rest1)
            in case (predicate, closing) of
               ("EQ", "]")    -> FilterByPredicate Equal <$> toIndex p ixTxt
               ("LT", "]")    -> FilterByPredicate LesserThan <$> toIndex p ixTxt
               ("LTE", "]")   -> FilterByPredicate LesserThanEqual <$> toIndex p ixTxt
               ("GT", "]")    -> FilterByPredicate GreaterThan <$> toIndex p ixTxt
               ("GTE", "]")   -> FilterByPredicate GreaterThanEqual <$> toIndex p ixTxt
               ("RANGE", "]") -> parseRangeQuery ixTxt
               _              -> Nothing

    -- Tries to parse a query by index.
    parseIndexQuery :: Maybe (FilterOperation ix a)
    parseIndexQuery = FilterByIndex <$> toIndex p txt

    -- Tries to parse a range query of the form RANGE[from,to].
    parseRangeQuery :: Text -> Maybe (FilterOperation ix a)
    parseRangeQuery fromTo =
        case bimap identity (T.drop 1) (T.breakOn "," fromTo) of
            (_, "")    -> Nothing
            (from, to) -> FilterByRange <$> toIndex p from <*> toIndex p to
