{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE RankNTypes                #-}
module Cardano.Wallet.API.Request.Filter where

import           Universum

import           Cardano.Wallet.API.V1.Types
import qualified Data.List as List
import           Data.String.Conv (toS)
import           Data.Typeable (Typeable)
import           GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

import           Network.HTTP.Types (parseQueryText)
import           Network.Wai (rawQueryString)
import           Servant
import           Servant.Server.Internal

-- DB STUFF

class IsIndexOf a idx | a -> idx where
    toIndex :: Proxy a -> Text -> Maybe idx

instance IsIndexOf Wallet WalletId where
    toIndex _ x = Just (WalletId x)

-- WEB STUFF

-- An (untyped) Query on the data model
data FilterOperation ix a =
      FilterByIndex ix
    | FilterByPredicate Ordering ix
    | FilterNoOp

data SortOperation a =
    SortBy Text

-- | Represents a filter operation on the data model.
-- Examples:
--   *    `wallet_id=DEADBEEF`.
--   *    `balance=GT[10]`
data FilterBy (sym :: Symbol) (r :: *) deriving Typeable

-- | Represents a sort operation on the data model.
-- Examples:
--   *    `sort_by=balance`.
data SortBy  (sym :: Symbol) deriving Typeable


type family FilterParam (sym :: Symbol) (r :: *) :: * where
    FilterParam "wallet_id" Wallet = WalletId


parseFilterOperation :: forall a ix. (IsIndexOf a ix)
                     => Proxy a
                     -> Proxy ix
                     -> Text
                     -> Either Text (FilterOperation ix a)
parseFilterOperation p Proxy txt = case toIndex p txt of
    Nothing  -> Right $ FilterNoOp
    Just idx -> Right $ FilterByIndex idx

instance (HasServer subApi ctx
         , KnownSymbol sym
         , FilterParam sym res ~ ix
         , IsIndexOf res ix
         ) => HasServer (FilterBy sym res :> subApi) ctx where
    type ServerT (FilterBy sym res :> subApi) m = FilterOperation (FilterParam sym res) res -> ServerT subApi m
    route Proxy context subserver =
        let querytext req = parseQueryText $ rawQueryString req
            paramName     = toText $ symbolVal (Proxy @sym)
            parseParam req =
              case List.lookup paramName (querytext req) of
                Nothing       -> return FilterNoOp
                Just Nothing  -> return FilterNoOp
                Just (Just v) ->
                  case parseFilterOperation (Proxy @res) (Proxy @ix) v of
                      Left e -> delayedFailFatal err400
                          { errBody = toS $ "Error parsing filter query " <> paramName <> " failed: " <> e
                          }

                      Right filterOp -> return filterOp
            delayed = addParameterCheck subserver . withRequest $ \req ->
                        parseParam req

        in route (Proxy :: Proxy subApi) context delayed

    hoistServerWithContext _ ct hoist' s = hoistServerWithContext (Proxy @subApi) ct hoist' . s
