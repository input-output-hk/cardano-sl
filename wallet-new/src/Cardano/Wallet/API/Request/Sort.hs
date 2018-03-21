{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE RankNTypes                #-}

module Cardano.Wallet.API.Request.Sort where

import qualified Prelude
import           Universum

import qualified Data.Text as T
import qualified Data.Text.Buildable
import           Data.Typeable
import           Data.Typeable (Typeable)
import           Formatting (bprint, build, builder, stext, (%))
import qualified Generics.SOP as SOP
import           GHC.TypeLits (KnownSymbol, symbolVal)
import           Network.HTTP.Types (parseQueryText)
import           Network.Wai (Request, rawQueryString)
import           Pos.Util.Servant (HasLoggingServer (..), LoggingApiRec, addParamLogInfo)
import           Serokell.Util.ANSI (Color (..), colorizeDull)
import           Servant
import           Servant.Client
import           Servant.Client.Core (appendToQueryString)
import           Servant.Server.Internal

import           Cardano.Wallet.API.Indices
import           Cardano.Wallet.API.Types
import           Cardano.Wallet.TypeLits (KnownSymbols, symbolVals)

-- | Represents a sort operation on the data model.
--
-- The first type parameter is a type level list that describes the
-- available types for sorting on. The name of the index is given by the
-- 'IndexToQueryParam' type family for the resource and type.
--
-- @
-- 'SortBy' '[ WalletId, Coin ] Wallet
-- @
--
-- The above combinator would permit query parameters that look like these
-- examples:
--
-- * @sort_by=ASC[id]@.
-- * @sort_by=DESC[balance]@
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
data SortBy (params :: [*]) (resource :: *)
    deriving Typeable

-- | The direction for the sort operation.
data SortDirection =
      SortAscending
    | SortDescending
    deriving (Eq, Show)

renderSortDir :: SortDirection -> Text
renderSortDir sd = case sd of
    SortAscending  -> "ASC"
    SortDescending -> "DESC"

instance Buildable SortDirection where
    build SortAscending  = "asc."
    build SortDescending = "desc."

-- | A sort operation on an index @ix@ for a resource 'a'.
data SortOperation ix a
    = SortByIndex SortDirection (Proxy ix)
    -- ^ Standard sort by index (e.g. sort_by=balance).
    deriving Eq

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

instance Show (SortOperation ix a) where
    show (SortByIndex dir _) = "SortByIndex[" <> show dir <> "]"

-- | A "bag" of sort operations, where the index constraint are captured in
-- the inner closure of 'SortOp'.
data SortOperations a where
    NoSorts  :: SortOperations a
    SortOp   :: IndexRelation a ix
             => SortOperation ix a
             -> SortOperations a
             -> SortOperations a

instance Eq (SortOperations a) where
    NoSorts == NoSorts =
       True
    SortOp (sop0 :: SortOperation ix0 a) rest0 == SortOp (sop1 :: SortOperation ix1 a) rest1 =
        case eqT @ix0 @ix1 of
            Nothing ->
                False
            Just Refl ->
                sop0 == sop1 && rest0 == rest1
    _ == _ =
        False

instance Show (SortOperations a) where
    show = show @_ @[Text] . flattenSortOperations show

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

-- | Handy helper function to convert 'SortOperation'(s) into list.
flattenSortOperations :: (forall (ix :: *). SortOperation ix a -> b)
                      -> SortOperations a
                      -> [b]
flattenSortOperations _     NoSorts       = mempty
flattenSortOperations trans (SortOp f fs) = trans f : flattenSortOperations trans fs

-- | Handy typeclass to reconcile type and value levels by building a list of 'SortOperation' out of
-- a type level list.
class ToSortOperations (ixs :: [*]) a where
  toSortOperations :: Request -> proxy ixs -> SortOperations a

instance Indexable' a => ToSortOperations ('[]) a where
  toSortOperations _ _ = NoSorts

instance ( IndexRelation a ix
         , ToSortOperations ixs a
         , IndexToQueryParam a ix ~ sym
         )
         => ToSortOperations (ix ': ixs) a where
    toSortOperations req _ =
        foldr (either (flip const) SortOp) rest
            . map (parseSortOperation (Proxy @a) (Proxy @ix))
            . mapMaybe snd
            . filter (("sort_by" ==) . fst)
            . parseQueryText
            $ rawQueryString req
      where
        rest = toSortOperations req (Proxy @ ixs)

-- | Servant's 'HasServer' instance telling us what to do with a type-level specification of a sort operation.
instance ( HasServer subApi ctx
         , syms ~ ParamNames res params
         , KnownSymbols syms
         , ToSortOperations params res
         , SOP.All (ToIndex res) params
         ) => HasServer (SortBy params res :> subApi) ctx where

    type ServerT (SortBy params res :> subApi) m = SortOperations res -> ServerT subApi m
    hoistServerWithContext _ ct hoist' s = hoistServerWithContext (Proxy @subApi) ct hoist' . s

    route Proxy context subserver =
        let delayed = addParameterCheck subserver . withRequest $ \req ->
                        return $ toSortOperations req (Proxy @params)

        in route (Proxy :: Proxy subApi) context delayed

-- | Logs all non-identity sorting parameters.
--
-- Example: @filter: asc id, desc balance@.
--
-- Default implementation of this instance for 'SortBy params res'
-- (note OVERLAPPING) would print logs in format of
-- "<some name dependant on 'syms'>: <prettified SortOperations>".
-- With current implementation of 'SortOperations' the best logs we can get
-- would be like "filter: asc, desc, none",
-- or "filter (id, created_at, balance): asc, desc, none",
-- because 'SortOperations' itself doesn't remember name of parameter it sorts
-- upon and those logs seem a bit cumbersome with growth of number of parameters
-- we allow to sort on.
-- Thus custom instance which cuts off "none"s is used.
instance {-# OVERLAPPING #-}
         ( HasLoggingServer config subApi ctx
         , ParamNames res params ~ syms
         , KnownSymbols syms
         , ToSortOperations params res
         , SOP.All (ToIndex res) params
         ) =>
         HasLoggingServer config (SortBy params res :> subApi) ctx where
    routeWithLog =
        mapRouter @(SortBy params res :> LoggingApiRec config subApi) route $
            \(paramsInfo, f) sortOps ->
            (updateParamsInfo sortOps paramsInfo, f sortOps)
      where
        updateParamsInfo sortOps =
            addParamLogInfo $ \_sl -> colorizeDull White $ buildSortOps sortOps
        allParams = map toText $ symbolVals (Proxy @syms)
        buildSortOps sortOps =
            let builtOps = flattenSortOperations buildSortOp sortOps
                namedOps = zipWith mergeNameAndOp allParams builtOps
                activeOps = catMaybes namedOps
            in  case activeOps of
                    [] -> "no sort"
                    ss -> pretty . mconcat $ intersperse ", " ss
        buildSortOp (SortByIndex sortType _) = Just $ bprint build sortType
        mergeNameAndOp _ Nothing      = Nothing
        mergeNameAndOp name (Just op) = Just $ bprint (builder%" "%stext) op name

-- | Parse the incoming HTTP query param into a 'SortOperation', failing if the input is not a valid operation.
parseSortOperation
    :: forall a ix sym
    . ( ToIndex a ix
      , IndexToQueryParam a ix ~ sym
      , KnownSymbol sym
      )
    => Proxy a
    -> Proxy ix
    -> Text
    -> Either Text (SortOperation ix a)
parseSortOperation _ ix@Proxy value =
    case (predicate, closing, ixTxt == key) of
        ("ASC", "]", True) ->
            Right $ SortByIndex SortAscending  ix
        ("DES", "]", True) ->
            Right $ SortByIndex SortDescending ix
        (mk, "", _) | mk == key ->
            Right $ SortByIndex SortDescending ix -- default sorting.
        _ ->
            Left $ mconcat
                [ "Failed to parse sort operation: '"
                , value
                , "'."
                ]
  where
    key                = toText $ symbolVal (Proxy @sym)
    (predicate, rest1) = T.breakOn "[" value
    (ixTxt, closing)   = T.breakOn "]" (T.drop 1 rest1)

instance
    ( HasClient m next
    , KnownSymbols syms
    , SOP.All (ToIndex res) params
    , syms ~ ParamNames res params
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
