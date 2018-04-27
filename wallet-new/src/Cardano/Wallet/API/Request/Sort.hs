{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE RankNTypes                #-}

module Cardano.Wallet.API.Request.Sort where

import qualified Prelude
import           Universum

import qualified Data.Text as T
import qualified Data.Text.Buildable
import           Data.Typeable
import           Formatting (bprint, build, formatToString, sformat)
import qualified Generics.SOP as SOP
import           GHC.TypeLits (KnownSymbol, symbolVal)
import           Network.HTTP.Types (parseQueryText)
import           Network.Wai (Request, rawQueryString)
import           Pos.Util.LogSafe (BuildableSafeGen (..), SecureLog (..), buildSafe, secure,
                                   unsecure)
import           Pos.Util.Servant (ApiCanLogArg (..), ApiHasArgClass (..))
import           Servant
import           Servant.Client
import           Servant.Client.Core (appendToQueryString)
import           Servant.Server.Internal

import           Cardano.Wallet.API.Indices

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

instance Buildable SortDirection where
    build = \case
        SortAscending  -> "ASC"
        SortDescending -> "DESC"


-- | A sort operation on an index @ix@ for a resource 'a'.
data SortOperation ix a
    = SortByIndex SortDirection (Proxy ix)
    -- ^ Standard sort by index (e.g. sort_by=balance).
    deriving Eq

instance (Buildable (SortOperation ix a)) => Show (SortOperation ix a) where
    show = formatToString build

instance (IndexToQueryParam a ix ~ sym , KnownSymbol sym) =>
    ToHttpApiData (SortOperation ix a) where
    toQueryParam = \case
        SortByIndex dir _ ->
            let
                ix = toText (symbolVal (Proxy @sym))
            in
                mconcat [ sformat build dir , "[" , ix , "]" ]

instance (ToHttpApiData (SortOperation ix a)) =>
    BuildableSafeGen (SortOperation ix a) where
    buildSafeGen _ = bprint build . toQueryParam

instance (BuildableSafeGen (SortOperation ix a)) =>
    Buildable (SortOperation ix a) where
    build = buildSafeGen unsecure

instance (BuildableSafeGen (SortOperation ix a)) =>
    Buildable (SecureLog (SortOperation ix a)) where
    build = buildSafeGen secure . getSecureLog


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
    show = formatToString build

instance BuildableSafeGen (SortOperations a) where
    buildSafeGen _ NoSorts =
        "-"
    buildSafeGen _ (SortOp op NoSorts) =
        bprint build op
    buildSafeGen _ (SortOp op rest) =
        bprint build op <> ", " <> bprint build rest

instance Buildable (SortOperations a) where
    build = buildSafeGen unsecure

instance Buildable (SecureLog (SortOperations a)) where
    build = buildSafeGen secure . getSecureLog


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
         , ToSortOperations params res
         , SOP.All (ToIndex res) params
         ) => HasServer (SortBy params res :> subApi) ctx where

    type ServerT (SortBy params res :> subApi) m = SortOperations res -> ServerT subApi m
    hoistServerWithContext _ ct hoist' s = hoistServerWithContext (Proxy @subApi) ct hoist' . s

    route Proxy context subserver =
        let delayed = addParameterCheck subserver . withRequest $ \req ->
                        return $ toSortOperations req (Proxy @params)

        in route (Proxy :: Proxy subApi) context delayed

instance ApiHasArgClass (SortBy params res) where
    type ApiArg (SortBy params res) = SortOperations res
    apiArgName _ = "sort_by"

instance ApiCanLogArg (SortBy params res) where
    toLogParamInfo _ param = \sl -> sformat (buildSafe sl) param


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
