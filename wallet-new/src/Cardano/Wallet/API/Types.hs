{-- Types shared between different API versions. --}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Cardano.Wallet.API.Types where

import           Universum

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Default            (Default (..))
import qualified Data.Text               as T
import           GHC.TypeLits
import qualified Servant.Server.Internal as SI

import           Servant
import           Servant.API.Sub         ((:>))
import           Servant.Swagger
import           Test.QuickCheck

--
-- Utilities
--
-- NOTE: Many things here are stolen from `Pos.Util.Servant` of `cardano-sl` package.
-- I didn't simply import it because I don't want to introduce `cardano-sl` as a dependency on
-- stage of API prototyping, as it would slow down rebuilds of the prototype.
-- In the future we should import this from `cardano-sl` or move `Pos.Util.Servant` module
-- to `wallet-new` altogether.

-- | `inRouteServer` is helper function used in order to transform one `HasServer`
-- instance to another. It can be used to introduce custom request params type.
-- See e. g. `WithDefaultApiArg` as an example of usage
inRouteServer
    :: forall api api' ctx env.
       (Proxy api -> SI.Context ctx -> SI.Delayed env (Server api) -> SI.Router env)
    -> (Server api' -> Server api)
    -> (Proxy api' -> SI.Context ctx -> SI.Delayed env (Server api') -> SI.Router env)
inRouteServer routing f = \_ ctx delayed -> routing Proxy ctx (fmap f delayed)

-- | `ApiHasArg*` types help to reflect on names and types of arguments of API methods,
-- specified by `Capture`s, `QueryParam`s, etc. These types are used to construct more
-- complex useful types such as `WithDefaultApiArg`.
class ApiHasArgClass apiType a where
    -- | For arguments-specifiers of API, get argument type.
    type ApiArg (apiType :: * -> *) a :: *
    type ApiArg apiType a = a

    -- | Name of argument.
    -- E.g. name of argument specified by @Capture "nyan"@ is /nyan/.
    apiArgName :: Proxy (apiType a) -> String
    default apiArgName
        :: forall n someApiType. (KnownSymbol n, someApiType n ~ apiType)
        => Proxy (someApiType n a) -> String
    apiArgName _ = symbolVal (Proxy @n)

type ApiHasArgInvariant apiType a res =
    Server (apiType a :> res) ~ (ApiArg apiType a -> Server res)

type ApiHasArg apiType a res =
    ( ApiHasArgClass apiType a
    , ApiHasArgInvariant apiType a res
    )

instance KnownSymbol s => ApiHasArgClass (Capture s) a
instance KnownSymbol s => ApiHasArgClass (QueryParam s) a where
    type ApiArg (QueryParam s) a = Maybe a
instance KnownSymbol s => ApiHasArgClass (QueryParams s) a where
    type ApiArg (QueryParams s) a = [a]
instance KnownSymbol s => ApiHasArgClass (Header s) a where
    type ApiArg (Header s) a = Maybe a
instance ApiHasArgClass (ReqBody ct) a where
    apiArgName _ = "request body"

-- | `Unmaybe` and `UnmaybeArg` type families are necessary to
-- get `a` from `Maybe a` in a way which doesn't make GHC mad.
-- Used in definition of `ApiHasArgClass` and `HasServer` instances for
-- `WithDefaultApiArg`
type family Unmaybe a where
    Unmaybe (Maybe a) = a

type family UnmaybeArg a where
    UnmaybeArg (Maybe a -> b) = a -> b

--
-- Specifying API method parameters with default values
--

-- | `WithDefaultApiArg` type is used to specify API parameters,
-- which are optional but have default values. It can be applied to every
-- API specifier which yields `Maybe a` as argument type.
data WithDefaultApiArg (argType :: * -> *) a

instance (ApiHasArgClass apiType a, ApiArg apiType a ~ Maybe b) =>
         ApiHasArgClass (WithDefaultApiArg apiType) a where
    type ApiArg (WithDefaultApiArg apiType) a = Unmaybe (ApiArg apiType a)
    apiArgName _ = apiArgName (Proxy @(apiType a))

instance ( HasServer (apiType a :> res) ctx
         , Server (apiType a :> res) ~ (Maybe c -> d)
         , Default c
         ) =>
         HasServer (WithDefaultApiArg apiType a :> res) ctx where
    type ServerT (WithDefaultApiArg apiType a :> res) m =
        UnmaybeArg (ServerT (apiType a :> res) m)
    route =
        inRouteServer @(apiType a :> res) route $
        \f a -> f $ fromMaybe def a

instance HasSwagger (apiType a :> res) =>
    HasSwagger (WithDefaultApiArg apiType a :> res) where
    toSwagger _ = toSwagger (Proxy @(apiType a :> res))

-- | Type aliases for query params and headers which have default values.
type DQueryParam s a = WithDefaultApiArg (QueryParam s) a
type DHeader s a = WithDefaultApiArg (Header s) a

--
-- Providing one argument via different methods
--
type family MergeArgs a where
    MergeArgs (a -> a -> b) = a -> b

-- | `AlternativeApiArg` is used when we want to provide a way
-- to pass one argument via two ways, e. g. via request header or query parameter.
data AlternativeApiArg (argA :: * -> *) (argB :: * -> *) a

instance ( ApiHasArgClass argA a
         , ApiHasArgClass argB a
         , ApiArg argA a ~ ApiArg argB a
         ) =>
         ApiHasArgClass (AlternativeApiArg argA argB) a where
    type ApiArg (AlternativeApiArg argA argB) a = ApiArg argA a
    apiArgName _ = apiArgName (Proxy @(argA a))

instance ( HasServer (argA a :> argB a :> res) ctx
         , Server (argA a :> argB a :> res) ~ (t b -> t b -> d)
         , Alternative t
         ) =>
         HasServer (AlternativeApiArg argA argB a :> res) ctx where
    type ServerT (AlternativeApiArg argA argB a :> res) m =
        MergeArgs (ServerT (argA a :> argB a :> res) m)
    route =
        inRouteServer @(argA a :> argB a :> res) route $
        \f a b -> f $ a <|> b

instance HasSwagger (argA a :> argB a :> res) =>
         HasSwagger (AlternativeApiArg argA argB a :> res) where
    toSwagger _ = toSwagger (Proxy @(argA a :> argB a :> res))

--
-- Types
--

-- | An empty type which can be used to inject the Swagger summary (i.e. a short, 120 chars
-- description of what an endpoint should do) at the type level, directly in the Servant API.
data Summary (sym :: Symbol)

-- | An empty type which can be used to inject Swagger tags at the type level,
-- directly in the Servant API.
data Tags (tags :: [Symbol])

-- | The API version. V0 represents the "legacy" API, whereas V1 is the API version currently
-- in use.
data APIVersion = V0
                | V1
                deriving (Eq, Show, Enum, Bounded)

data WalletVersion = WalletVersion {
      ver_version :: APIVersion
    , ver_gitRev  :: T.Text
    -- ^ The current git revision
    } deriving (Show, Eq)

--
-- Instances
--

-- | Instance of `HasServer` which erases a `Summary` from its routing,
-- as the latter is needed only for Swagger.
instance (HasServer subApi context) => HasServer (Summary desc :> subApi) context where
  type ServerT (Summary desc :> subApi) m = ServerT subApi m
  route _ = route (Proxy @ subApi)

-- | Instance of `HasServer` which erases the `Tags` from its routing,
-- as the latter is needed only for Swagger.
instance (HasServer subApi context) => HasServer (Tags tags :> subApi) context where
  type ServerT (Tags tags :> subApi) m = ServerT subApi m
  route _ = route (Proxy @ subApi)

instance Arbitrary APIVersion where
  arbitrary = elements [minBound .. maxBound]

instance ToJSON APIVersion where
    toJSON V0 = String "v0"
    toJSON V1 = String "v1"

deriveToJSON defaultOptions { fieldLabelModifier = drop 4 } ''WalletVersion

instance Arbitrary WalletVersion where
    arbitrary = WalletVersion <$> arbitrary <*> pure "6f1131adca2f0bc6d24c9181cabd2b9e0704fd79"
