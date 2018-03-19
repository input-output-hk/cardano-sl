{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE RankNTypes                #-}

module Cardano.Wallet.API.Request.Sort where

import qualified Prelude
import           Universum

import           Cardano.Wallet.API.Indices
import qualified Cardano.Wallet.API.Request.Parameters as Param
import           Cardano.Wallet.API.V1.Types
import           Cardano.Wallet.TypeLits (KnownSymbols, symbolVals)
import qualified Data.List as List
import qualified Data.Text as T
import           Data.Typeable
import qualified Generics.SOP as SOP
import           GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import           Network.HTTP.Types (parseQueryText)
import           Network.Wai (Request, rawQueryString)
import           Servant
import           Servant.Client
import           Servant.Client.Core (appendToQueryString)
import           Servant.Server.Internal

import           Cardano.Wallet.API.Indices
import           Cardano.Wallet.API.V1.Types
import           Cardano.Wallet.TypeLits (KnownSymbols, symbolVals)
import           Pos.Core as Core

-- | Represents a sort operation on the data model.
--
-- The first type parameter is a type level list that pairs the query
-- parameter string with the expected parsed type. The second type
-- parameter describes the resource that is being sorted.
--
-- @
-- 'SortBy' '[ "id" ?= WalletId, "balance" ?= Coin ] Wallet
-- @
--
-- The above combinator would permit query parameters that look like these
-- examples:
--
-- * @sort_by=ASC[id]@.
-- * @sort_by=DESC[balance]@
--
-- In order for this to work, you need to ensure that the type family
-- 'IndexToQueryParam' has an entry for each @'[symbol ?= typ] resource@.
-- Otherwise, the client and server won't know how to associate the data
-- and construct requests.
data SortBy (params :: [*]) (resource :: *)
    deriving Typeable

-- | The direction for the sort operation.
data SortDirection =
      SortAscending
    | SortDescending
    deriving Show

renderSortDir :: SortDirection -> Text
renderSortDir sd = case sd of
    SortAscending  -> "ASC"
    SortDescending -> "DESC"

-- | A sort operation on an index @ix@ for a resource 'a'.
data SortOperation ix a =
      SortByIndex SortDirection (Proxy ix)
    -- ^ Standard sort by index (e.g. sort_by=balance).

instance
    ( IndexToQueryParam a ix ~ sym
    , KnownSymbol sym
    ) => ToHttpApiData (SortOperation ix a) where
    toQueryParam sop =
        case sop of
            SortByIndex sortDir _ ->
                mconcat
                    [ renderSortDir sortDir
                    , "["
                    , toText (symbolVal (Proxy @sym))
                    , "]"
                    ]

instance Show (SortOperations a) where
    show = show . flattenSortOperations

-- | A "bag" of sort operations, where the index constraint are captured in
-- the inner closure of 'SortOp'.
data SortOperations a where
    NoSorts  :: SortOperations a
    SortOp   :: (Indexable' a, IsIndexOf' a ix, ToIndex a ix, Typeable ix, KnownSymbol (IndexToQueryParam a ix))
             => SortOperation ix a
             -> SortOperations a
             -> SortOperations a

findMatchingSortOp
    :: forall (needle :: *) (a :: *)
    . Typeable needle
    => SortOperations a
    -> Maybe (SortOperation needle a)
findMatchingSortOp NoSorts = Nothing
findMatchingSortOp (SortOp (sop :: SortOperation ix a) rest) =
    case eqT @needle @ix of
        Just Refl -> pure sop
        Nothing   -> findMatchingSortOp rest

instance Show (SortOperation ix a) where
    show (SortByIndex dir _) = "SortByIndex[" <> show dir <> "]"

-- | Handy helper function to show opaque 'FilterOperation'(s), mostly for
-- debug purposes.
flattenSortOperations :: SortOperations a -> [String]
flattenSortOperations NoSorts       = mempty
flattenSortOperations (SortOp f fs) = show f : flattenSortOperations fs

-- | This is a slighly boilerplat-y type family which maps symbols to
-- indices, so that we can later on reify them into a list of valid indices.
-- In case we want to sort on _all_ the indices, it might make sense having an
-- entry like the following:
--
--    SortParams '["wallet_id", "balance"] Wallet = IndicesOf Wallet
--
-- In the case of a 'Wallet', for example, sorting by @wallet_id@ doesn't have
-- much sense, so we restrict ourselves.
type family SortParams (syms :: [Symbol]) (r :: *) :: [*] where
    SortParams '[Param.Balance] Wallet = '[Core.Coin]
    SortParams '[Param.CreatedAt] Transaction = '[Core.Timestamp]

-- | Handy typeclass to reconcile type and value levels by building a list of 'SortOperation' out of
-- a type level list.
class ToSortOperations (ixs :: [*]) a where
  toSortOperations :: Request -> [Text] -> proxy ixs -> SortOperations a

instance Indexable' a => ToSortOperations ('[]) a where
  toSortOperations _ _ _ = NoSorts

instance ( Indexable' a
         , IsIndexOf' a ix
         , ToIndex a ix
         , ToSortOperations ixs a
         , Typeable ix
         , KnownSymbol sym
         , IndexToQueryParam a ix ~ sym
         )
         => ToSortOperations (ix ': ixs) a where
    toSortOperations _ [] _     =
        NoSorts
    toSortOperations req (key:xs) _ =
        foldr (either (flip const) SortOp) rest
            . map (parseSortOperation (Proxy @a) (Proxy @ix) . (,) key)
            . mapMaybe snd
            . filter (("sort_by" ==) . fst)
            . parseQueryText
            $ rawQueryString req
      where
        rest = toSortOperations req xs (Proxy @ ixs)

-- | Servant's 'HasServer' instance telling us what to do with a type-level specification of a sort operation.
instance ( HasServer subApi ctx
         , ixs ~ ParamTypes params
         , syms ~ ParamNames params
         , KnownSymbols syms
         , ToSortOperations ixs res
         , SOP.All (ToIndex res) ixs
         ) => HasServer (SortBy params res :> subApi) ctx where

    type ServerT (SortBy params res :> subApi) m = SortOperations res -> ServerT subApi m
    hoistServerWithContext _ ct hoist' s = hoistServerWithContext (Proxy @subApi) ct hoist' . s

    route Proxy context subserver =
        let allParams = map toText $ symbolVals (Proxy @syms)
            delayed = addParameterCheck subserver . withRequest $ \req ->
                        return $ toSortOperations req allParams (Proxy @ixs)

        in route (Proxy :: Proxy subApi) context delayed

-- | Parse the incoming HTTP query param into a 'SortOperation', failing if the input is not a valid operation.
parseSortOperation
    :: forall a ix sym
    . ( ToIndex a ix
      , IndexToQueryParam a ix ~ sym
      , KnownSymbol sym
      )
    => Proxy a
    -> Proxy ix
    -> (Text, Text)
    -> Either Text (SortOperation ix a)
parseSortOperation _ ix@Proxy (key,value) = case parseQuery of
    Nothing -> Left "Not a valid sort."
    Just f  -> Right f
  where
    parseQuery :: Maybe (SortOperation ix a)
    parseQuery =
        let (predicate, rest1) = (T.take 4 value, T.drop 4 value)
            (ixTxt, closing)   = T.breakOn "]" rest1
            in case (predicate, closing, ixTxt == key) of
               ("ASC[", "]", True) -> Just $ SortByIndex SortAscending  ix
               ("DES[", "]", True) -> Just $ SortByIndex SortDescending ix
               (_, _, True)        -> Just $ SortByIndex SortDescending ix -- default sorting.
               _                   -> Nothing

instance
    ( HasClient m next
    , KnownSymbols syms
    , SOP.All (ToIndex res) ixs
    , syms ~ ParamNames params
    , ixs ~ ParamTypes params
    )
    => HasClient m (SortBy params res :> next) where
    type Client m (SortBy params res :> next) =
        SortOperations res -> Client m next
    clientWithRoute pm _ r s =
        clientWithRoute pm (Proxy @next) (incorporate s r)
      where
        incorporate NoSorts = identity
        incorporate (SortOp (sop :: SortOperation ix res) rest) =
            incorporate rest .
                appendToQueryString
                    (toText (symbolVal (Proxy @(IndexToQueryParam res ix))))
                    (Just (toQueryParam sop))
