{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE RankNTypes                #-}
module Cardano.Wallet.API.Request.Filter where

import           Universum

import           Cardano.Wallet.API.V1.Types
import           Cardano.Wallet.TypeLits (KnownSymbols, symbolVals)
import qualified Data.List as List
import           Data.String.Conv (toS)
import           Data.Typeable (Typeable)
import qualified Generics.SOP as SOP
import           GHC.TypeLits (Symbol)
import qualified Pos.Core as Core

import           Network.HTTP.Types (parseQueryText)
import           Network.Wai (Request, rawQueryString)
import           Servant
import           Servant.Server.Internal

-- DB STUFF

type WalletIxs = '[WalletId, Core.Coin]

class IsIndexOf a idx where
    toIndex :: Proxy a -> Text -> Maybe idx

instance IsIndexOf Wallet WalletId where
    toIndex _ x = Just (WalletId x)

instance IsIndexOf Wallet Core.Coin where
    -- TODO: Temporary.
    toIndex _ x = Core.mkCoin <$> readMaybe (toS x)

-- WEB STUFF

data Index a where
    Index :: IsIndexOf a ix => ix -> Index a

data FilterOperations (a :: *) where
    NoFilters  :: FilterOperations a
    (:::) :: IsIndexOf a ix => FilterOperation ix a -> FilterOperations a -> FilterOperations a

infixr 5 :::

noFilters :: FilterOperations a
noFilters = NoFilters

-- A filter operation on the data model
data FilterOperation ix a =
      FilterByIndex ix
    | FilterByPredicate Ordering ix
    | FilterNoOp

class FilterBackend f a where
    filterData :: FilterOperations a -> f a -> f a

data SortOperation a =
    SortBy Text

-- | Represents a filter operation on the data model.
-- Examples:
--   *    `wallet_id=DEADBEEF`.
--   *    `balance=GT[10]`
data FilterBy (sym :: [Symbol]) (r :: *) deriving Typeable

-- | Represents a sort operation on the data model.
-- Examples:
--   *    `sort_by=balance`.
data SortBy  (sym :: Symbol) deriving Typeable

type family NonEmptyTF (xs :: [*]) :: Bool where
    NonEmptyTF '[] = 'False
    NonEmptyTF (x ': xs) = 'True

type family FilterParams (syms :: [Symbol]) (r :: *) :: [*] where
    FilterParams '["wallet_id", "balance"] Wallet = WalletIxs

parseFilterOperation :: forall a ix. (IsIndexOf a ix)
                     => Proxy a
                     -> Proxy ix
                     -> Text
                     -> Either Text (FilterOperation ix a)
parseFilterOperation p Proxy txt = case toIndex p txt of
    Nothing  -> Right $ FilterNoOp
    Just idx -> Right $ FilterByIndex idx

class ToFilterOperations (xs :: [*]) res where
  toFilterOperations :: Request -> [Text] -> proxy xs -> FilterOperations res

instance ToFilterOperations ('[]) res where
  toFilterOperations _ _ _ = NoFilters

instance (IsIndexOf res ix, ToFilterOperations ixs res) => ToFilterOperations (ix ': ixs) res where
  toFilterOperations req [] _     = toFilterOperations req [] (Proxy :: Proxy ixs)
  toFilterOperations req (x:xs) _ =
      case List.lookup x (parseQueryText $ rawQueryString req) of
          Nothing       ->  toFilterOperations req xs (Proxy @ ixs)
          Just Nothing  ->  toFilterOperations req xs (Proxy @ ixs)
          Just (Just v) ->  case parseFilterOperation (Proxy @res) (Proxy @ix) v of
                                Left _      -> toFilterOperations req xs (Proxy @ ixs)
                                Right newOp -> newOp ::: toFilterOperations req xs (Proxy @ ixs)

instance ( HasServer subApi ctx
         , FilterParams syms res ~ ixs
         , KnownSymbols syms
         , ToFilterOperations ixs res
         , SOP.All (IsIndexOf res) ixs
         ) => HasServer (FilterBy syms res :> subApi) ctx where

    type ServerT (FilterBy syms res :> subApi) m = FilterOperations res -> ServerT subApi m
    hoistServerWithContext _ ct hoist' s = hoistServerWithContext (Proxy @subApi) ct hoist' . s

    route Proxy context subserver =
        let allParams = map toText $ symbolVals (Proxy @syms)
            delayed = addParameterCheck subserver . withRequest $ \req ->
                        parseFilterParams req allParams (Proxy @ixs)

        in route (Proxy :: Proxy subApi) context delayed


parseFilterParams :: forall res ixs. (
                     SOP.All (IsIndexOf res) ixs
                  ,  ToFilterOperations ixs res
                  )
                  => Request
                  -> [Text]
                  -> Proxy ixs
                  -> DelayedIO (FilterOperations res)
parseFilterParams req params p = return $ toFilterOperations req params p
{-
  where
      -- go :: [Text] -> Proxy ixs -> FilterOperations ixs res -> DelayedIO (FilterOperations ixs res)
      go params (p :: Proxy '[]) ops = return ops
      go params (p :: Proxy (ix ': is)) ops =
          case params of
              [] -> return ops
              (p:ps) -> case List.lookup p (querytext req) of
                  Nothing       ->  go ps (Proxy @ is) (FilterNoOp ::: ops)
                  Just Nothing  ->  go ps (Proxy @ is) (FilterNoOp ::: ops)
                  Just (Just v) ->
                    case parseFilterOperation (Proxy @res) (Proxy @ix) v of
                        Left e -> delayedFailFatal err400
                            { errBody = toS $ "Error parsing filter query " <> paramName <> " failed: " <> e
                            }

                        Right filterOp -> go ps (Proxy @is) (filterOp ::: ops)
-}
