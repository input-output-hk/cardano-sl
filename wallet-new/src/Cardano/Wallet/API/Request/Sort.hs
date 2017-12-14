{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE RankNTypes                #-}
module Cardano.Wallet.API.Request.Sort where

import           Universum

import           Cardano.Wallet.API.Indices
import           Cardano.Wallet.API.V1.Types
import           Cardano.Wallet.TypeLits (KnownSymbols, symbolVals)
import qualified Data.List as List
import qualified Data.Text as T
import           Data.Typeable (Typeable)
import qualified Generics.SOP as SOP
import           GHC.TypeLits (Symbol)

import           Network.HTTP.Types (parseQueryText)
import           Network.Wai (Request, rawQueryString)
import           Servant
import           Servant.Server.Internal

-- | Represents a sort operation on the data model.
-- Examples:
--   *    `sort_by=balance`.
data SortBy (sym :: [Symbol]) (r :: *) deriving Typeable

data SortDirection =
      SortAscending
    | SortDescending

data SortOperation ix a =
      SortByIndex SortDirection ix
    | SortIdentity

-- | A "bag" of sort operations, where the index constraint are captured in
-- the inner closure of 'SortOp'.
data SortOperations a where
    NoSorts  :: SortOperations a
    SortOp   :: (Indexable' a, IsIndexOf' a ix, ToIndex a ix)
             => SortOperation ix a
             -> SortOperations a
             -> SortOperations a

-- | This is a slighly boilerplat-y type family which maps symbols to
-- indices, so that we can later on reify them into a list of valid indices.
type family SortParams (syms :: [Symbol]) (r :: *) :: [*] where
    SortParams '["wallet_id", "balance"] Wallet = IndicesOf Wallet

parseSortOperation :: forall a ix. (ToIndex a ix)
                   => Proxy a
                   -> Proxy ix
                   -> Text
                   -> Either Text (SortOperation ix a)
parseSortOperation p Proxy txt = case parsePredicateQuery <|> parseIndexQuery of
    Nothing -> Left "Not a valid sort."
    Just f  -> Right f
  where
    parsePredicateQuery :: Maybe (SortOperation ix a)
    parsePredicateQuery =
        let (predicate, rest1) = (T.take 4 txt, T.drop 4 txt)
            (ixTxt, closing)   = T.breakOn "]" rest1
            in case (predicate, closing) of
               ("ASC[", "]") -> SortByIndex SortAscending <$> toIndex p ixTxt
               ("DES[", "]") -> SortByIndex SortDescending <$> toIndex p ixTxt
               _             -> Nothing

    parseIndexQuery :: Maybe (SortOperation ix a)
    parseIndexQuery = SortByIndex SortAscending <$> toIndex p txt

class ToSortOperations (ixs :: [*]) a where
  toSortOperations :: Request -> [Text] -> proxy ixs -> SortOperations a

instance Indexable' a => ToSortOperations ('[]) a where
  toSortOperations _ _ _ = NoSorts

instance ( Indexable' a
         , IsIndexOf' a ix
         , ToIndex a ix
         , ToSortOperations ixs a
         )
         => ToSortOperations (ix ': ixs) a where
  toSortOperations req [] _     =
      let newOp = SortIdentity
      in SortOp (newOp :: SortOperation ix a) (toSortOperations req [] (Proxy :: Proxy ixs))
  toSortOperations req (x:xs) _ =
      case List.lookup x (parseQueryText $ rawQueryString req) of
          Nothing       ->
              let newOp = SortIdentity
              in SortOp (newOp :: SortOperation ix a) (toSortOperations req xs (Proxy @ ixs))
          Just Nothing  ->
              let newOp = SortIdentity
              in SortOp (newOp :: SortOperation ix a) (toSortOperations req xs (Proxy @ ixs))
          Just (Just v) ->
              case parseSortOperation (Proxy @a) (Proxy @ix) v of
                  Left _      ->
                      let newOp = SortIdentity
                      in SortOp (newOp :: SortOperation ix a) (toSortOperations req xs (Proxy @ ixs))
                  Right newOp -> newOp `SortOp` toSortOperations req xs (Proxy @ ixs)

instance ( HasServer subApi ctx
         , SortParams syms res ~ ixs
         , KnownSymbols syms
         , ToSortOperations ixs res
         , SOP.All (ToIndex res) ixs
         ) => HasServer (SortBy syms res :> subApi) ctx where

    type ServerT (SortBy syms res :> subApi) m = SortOperations res -> ServerT subApi m
    hoistServerWithContext _ ct hoist' s = hoistServerWithContext (Proxy @subApi) ct hoist' . s

    route Proxy context subserver =
        let allParams = map toText $ symbolVals (Proxy @syms)
            delayed = addParameterCheck subserver . withRequest $ \req ->
                        parseSortParams req allParams (Proxy @ixs)

        in route (Proxy :: Proxy subApi) context delayed


parseSortParams :: forall a ixs. (
                     SOP.All (ToIndex a) ixs
                  ,  ToSortOperations ixs a
                  )
                  => Request
                  -> [Text]
                  -> Proxy ixs
                  -> DelayedIO (SortOperations a)
parseSortParams req params p = return $ toSortOperations req params p
