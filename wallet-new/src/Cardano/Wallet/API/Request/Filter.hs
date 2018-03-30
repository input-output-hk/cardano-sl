{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE RankNTypes                #-}

module Cardano.Wallet.API.Request.Filter where

import qualified Prelude
import           Universum

import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.Buildable
import           Data.Typeable
import           Formatting (bprint, build, formatToString, sformat, (%))
import qualified Generics.SOP as SOP
import           GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import           Pos.Util.LogSafe (BuildableSafe, BuildableSafeGen (..), SecureLog (..), buildSafe,
                                   secure, unsecure)
import           Pos.Util.Servant (ApiCanLogArg (..), ApiHasArgClass (..))

import           Network.HTTP.Types (parseQueryText)
import           Network.Wai (rawQueryString)
import           Servant
import           Servant.Client
import           Servant.Client.Core (appendToQueryString)
import           Servant.Server.Internal

import           Cardano.Wallet.API.Indices
import qualified Cardano.Wallet.API.Request.Parameters as Param
import           Cardano.Wallet.API.V1.Types

--
-- Filtering data
--

-- | A "bag" of filter operations, where the index constraint are captured in
-- the inner closure of 'FilterOp'.
data FilterOperations a where
    NoFilters  :: FilterOperations a
    FilterOp   :: ( IndexRelation a ix
                  , KnownSymbol (IndexToQueryParam a ix)
                  , BuildableSafe ix
                  , FromHttpApiData ix
                  , ToHttpApiData ix
                  , Eq ix
                  )
               => FilterOperation ix a
               -> FilterOperations a
               -> FilterOperations a

infixr 6 `FilterOp`

instance Show (FilterOperations a) where
    show = formatToString build

instance Eq (FilterOperations a) where
    NoFilters == NoFilters =
        True
    FilterOp (f0 :: FilterOperation ix0 a) rest0 == FilterOp (f1 :: FilterOperation ix1 a) rest1 =
        case eqT @ix0 @ix1 of
            Nothing ->
                False
            Just Refl ->
                f0 == f1 && rest0 == rest1
    _ == _ =
        False

instance BuildableSafeGen (FilterOperations a) where
    buildSafeGen _ NoFilters =
        "-"
    buildSafeGen sl (FilterOp op NoFilters) =
        bprint (buildSafe sl) op
    buildSafeGen sl (FilterOp op rest) =
        bprint (buildSafe sl) op <> ", " <> bprint (buildSafe sl) rest

instance Buildable (FilterOperations a) where
    build = buildSafeGen unsecure

instance Buildable (SecureLog (FilterOperations a)) where
    build = buildSafeGen secure . getSecureLog


-- A custom ordering for a 'FilterOperation'. Conceptually theh same as 'Ordering' but with the ">=" and "<="
-- variants.
data FilterOrdering =
      Equal
    | GreaterThan
    | GreaterThanEqual
    | LesserThan
    | LesserThanEqual
    deriving (Show, Eq, Enum, Bounded)

renderFilterOrdering :: FilterOrdering -> Text
renderFilterOrdering = sformat build

instance Buildable FilterOrdering where
    build = \case
        Equal            -> "EQ"
        GreaterThan      -> "GT"
        GreaterThanEqual -> "GTE"
        LesserThan       -> "LT"
        LesserThanEqual  -> "LTE"


-- A filter operation on the data model
data FilterOperation ix a =
      FilterByIndex ix
    -- ^ Filter by index (e.g. equal to)
    | FilterByPredicate FilterOrdering ix
    -- ^ Filter by predicate (e.g. lesser than, greater than, etc.)
    | FilterByRange ix ix
    -- ^ Filter by range, in the form [from,to]
    | FilterIn [ix]
    deriving Eq

instance (BuildableSafe ix, sym ~ IndexToQueryParam a ix, KnownSymbol sym) => Show (FilterOperation ix a) where
    show = formatToString build

instance ToHttpApiData ix => ToHttpApiData (FilterOperation ix a) where
    toQueryParam = renderFilterOperation

instance (BuildableSafe ix, sym ~ IndexToQueryParam a ix, KnownSymbol sym) =>
    BuildableSafeGen (FilterOperation ix a) where
    buildSafeGen sl (FilterByIndex ix) =
        bprint (build%"="%buildSafe sl) (symbolVal (Proxy @sym)) ix
    buildSafeGen sl (FilterByPredicate o ix) =
        bprint (build%"="%build%"["%buildSafe sl%"]") (symbolVal (Proxy @sym)) o ix
    buildSafeGen sl (FilterByRange lo hi) =
        bprint (build%"=RANGE["%buildSafe sl%","%buildSafe sl%"]") (symbolVal (Proxy @sym)) lo hi
    buildSafeGen sl (FilterIn ixs) =
        bprint (build % "=IN[" % build % "]")
            (symbolVal (Proxy @sym))
            bps
      where
        bps = mconcat
            . List.intersperse ","
            $ map (bprint (buildSafe sl)) ixs



instance (BuildableSafeGen (FilterOperation ix a)) => Buildable (FilterOperation ix a) where
    build = buildSafeGen unsecure

renderFilterOperation :: ToHttpApiData ix => FilterOperation ix a -> Text
renderFilterOperation = \case
    FilterByIndex ix ->
        toQueryParam ix
    FilterByPredicate p ix ->
        mconcat [renderFilterOrdering p, "[", toQueryParam ix, "]"]
    FilterByRange lo hi  ->
        mconcat ["RANGE", "[", toQueryParam lo, ",", toQueryParam hi, "]"]
    FilterIn ixs ->
        "IN[" <> T.intercalate "," (map toQueryParam ixs) <> "]"

instance (BuildableSafeGen (FilterOperation ix a)) => Buildable (SecureLog (FilterOperation ix a)) where
    build = buildSafeGen secure . getSecureLog

findMatchingFilterOp
    :: forall needle a
    . Typeable needle
    => FilterOperations a
    -> Maybe (FilterOperation needle a)
findMatchingFilterOp filters =
    case filters of
        NoFilters ->
            Nothing
        FilterOp (fop :: FilterOperation ix a) rest ->
            case eqT @ix @needle of
                Just Refl ->
                    pure fop
                Nothing ->
                    findMatchingFilterOp rest

-- | Represents a filter operation on the data model.
--
-- The first type parameter is a type level list that describes the
-- available types for Filtering on. The name of the index is given by the
-- 'IndexToQueryParam' type family for the resource and type.
--
-- @
-- 'FilterBy' '[ WalletId, Coin ] Wallet
-- @
--
-- The above combinator would permit query parameters that look like these
-- examples:
--
-- * @id=DEADBEEF@.
-- * @balance=GT[10]@
-- * @balance=RANGE[0,10]@
--
-- In order for this to work, you need to ensure that the type family
-- 'IndexToQueryParam' has an entry for each type for the resource.
--
-- The instances that enable the above lines are:
--
-- @
--     'IndexToQueryParam' 'Wallet' 'WalletId' = "id"
--     'IndexToQueryParam' 'Wallet' 'Coin'     = "balance"
-- @
data FilterBy (params :: [*]) (resource :: *)
    deriving Typeable

-- | This is a slighly boilerplat-y type family which maps symbols to
-- indices, so that we can later on reify them into a list of valid indices.
type family FilterParams (syms :: [Symbol]) (r :: *) :: [*] where
    FilterParams '[Param.WalletId, Param.Balance] Wallet = IndicesOf Wallet
    FilterParams '[Param.Id, Param.CreatedAt] Transaction = IndicesOf Transaction

class ToFilterOperations (ixs :: [*]) a where
  toFilterOperations :: [(Text, Maybe Text)] -> proxy ixs -> FilterOperations a

instance Indexable' a => ToFilterOperations ('[]) a where
  toFilterOperations _ _ = NoFilters

instance ( IndexRelation a ix
         , ToHttpApiData ix
         , FromHttpApiData ix
         , BuildableSafe ix
         , ToFilterOperations ixs a
         , sym ~ IndexToQueryParam a ix
         , KnownSymbol sym
         )
         => ToFilterOperations (ix ': ixs) a where
    toFilterOperations params _ =
        fromMaybe rest $ do
            v <- join $ List.lookup x params
            op <- rightToMaybe $ parseFilterOperation (Proxy @a) (Proxy @ix) v
            pure (FilterOp op rest)
      where
        rest = toFilterOperations params (Proxy @ ixs)
        x = toText $ symbolVal (Proxy @sym)

instance ( HasServer subApi ctx
         , ToFilterOperations params res
         , SOP.All (ToIndex res) params
         ) => HasServer (FilterBy params res :> subApi) ctx where

    type ServerT (FilterBy params res :> subApi) m = FilterOperations res -> ServerT subApi m
    hoistServerWithContext _ ct hoist' s = hoistServerWithContext (Proxy @subApi) ct hoist' . s

    route Proxy context subserver =
        let delayed = addParameterCheck subserver . withRequest $ \req ->
                          return $ toFilterOperations (parseQueryText $ rawQueryString req) (Proxy @params)
        in route (Proxy :: Proxy subApi) context delayed

-- | Defines name of @FilterBy syms res@
instance ApiHasArgClass (FilterBy params res) where
    type ApiArg (FilterBy params res) = FilterOperations res
    apiArgName _ = "filter_by"

-- | Defines how 'FilterBy' is logged by just refering to instance Buildable FilterOperations.
instance ApiCanLogArg (FilterBy params res) where
    toLogParamInfo _ param = \sl -> sformat (buildSafe sl) param

-- | Parse the filter operations, failing silently if the query is malformed.
-- TODO(adinapoli): we need to improve error handling (and the parsers, for
-- what is worth).
parseFilterOperation
    :: forall a ix
    . ToIndex a ix
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
               ("IN", "]")    -> parseInQuery ixTxt
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

    parseInQuery :: Text -> Maybe (FilterOperation ix a)
    parseInQuery =
        fmap FilterIn . traverse (toIndex p) . T.splitOn ","


instance
    ( HasClient m next
    , SOP.All (ToIndex res) params
    )
    => HasClient m (FilterBy params res :> next) where
    type Client m (FilterBy params res :> next) =
        FilterOperations res -> Client m next
    clientWithRoute pm _ req fs =
        clientWithRoute pm (Proxy @next) (incorporate fs)
      where
        incorporate =
            foldr (uncurry appendToQueryString) req . toQueryString

toQueryString :: FilterOperations a -> [(Text, Maybe Text)]
toQueryString NoFilters = []
toQueryString (FilterOp (fop :: FilterOperation ix a) rest) =
    ( toText (symbolVal (Proxy @(IndexToQueryParam a ix)))
    , Just (toQueryParam fop)
    ) : toQueryString rest
