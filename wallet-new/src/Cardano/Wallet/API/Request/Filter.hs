{-# LANGUAGE ConstraintKinds           #-}
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

import           Data.IxSet.Typed (Indexable (..), IsIndexOf)
import           Network.HTTP.Types (parseQueryText)
import           Network.Wai (Request, rawQueryString)
import           Servant
import           Servant.Server.Internal

-- DB STUFF

type WalletIxs = '[WalletId, Core.Coin]
type TransactionIxs = '[TxId]

class ToIndex a ix where
    -- | How to build this index from the input 'Text'.
    toIndex :: Proxy a -> Text -> Maybe ix

type family IndicesOf a :: [*] where
    IndicesOf Wallet      = WalletIxs
    IndicesOf Transaction = TransactionIxs

type Indexable' a    = Indexable (IndicesOf a) a
type IsIndexOf' a ix = IsIndexOf ix (IndicesOf a)

instance ToIndex Wallet WalletId where
    toIndex _ x = Just (WalletId x)

instance ToIndex Wallet Core.Coin where
    -- TODO: Temporary.
    toIndex _ x = Core.mkCoin <$> readMaybe (toS x)

-- WEB STUFF

-- | Represents a sort operation on the data model.
-- Examples:
--   *    `sort_by=balance`.
data SortBy  (sym :: Symbol) deriving Typeable

data SortOperation a =
    SortBy Text

data FilterOperations a where
    NoFilters  :: FilterOperations a
    FilterOp   :: (Indexable' a, IsIndexOf' a ix, ToIndex a ix)
               => FilterOperation ix a
               -> FilterOperations a
               -> FilterOperations a

-- A filter operation on the data model
data FilterOperation ix a =
      FilterByIndex ix
    | FilterByPredicate Ordering ix
    | FilterIdentity

-- | Represents a filter operation on the data model.
-- Examples:
--   *    `wallet_id=DEADBEEF`.
--   *    `balance=GT[10]`
data FilterBy (sym :: [Symbol]) (r :: *) deriving Typeable

type family FilterParams (syms :: [Symbol]) (r :: *) :: [*] where
    FilterParams '["wallet_id", "balance"] Wallet = IndicesOf Wallet

parseFilterOperation :: forall a ix. (ToIndex a ix)
                     => Proxy a
                     -> Proxy ix
                     -> Text
                     -> Either Text (FilterOperation ix a)
parseFilterOperation p Proxy txt = case toIndex p txt of
    Nothing  -> Left "This is not a filter."
    Just idx -> Right $ FilterByIndex idx

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
                        parseFilterParams req allParams (Proxy @ixs)

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
