{-- Types shared between different API versions. --}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs           #-}

module Cardano.Wallet.API.Types
       ( DQueryParam
       , DHeader
       , mapRouter
       , WithDefaultApiArg
       , AlternativeApiArg
       , Tags
       , WalletLoggingConfig
       ) where

import           Universum

import           Data.Default (Default (..))
import           Data.Reflection (Reifies (..))
import           GHC.TypeLits
import           Pos.Util.Servant (ApiLoggingConfig (..), HasLoggingServer (..), LoggingApiRec)
import qualified Servant.Server.Internal as SI

import           Servant
import           Servant.API.Sub ((:>))
import           Servant.Client

--
-- Utilities
--

-- | `mapRouter` is helper function used in order to transform one `HasServer`
-- instance to another. It can be used to introduce custom request params type.
-- See e. g. `WithDefaultApiArg` as an example of usage
mapRouter
    :: forall api api' ctx env.
       (Proxy api -> SI.Context ctx -> SI.Delayed env (Server api) -> SI.Router env)
    -> (Server api' -> Server api)
    -> (Proxy api' -> SI.Context ctx -> SI.Delayed env (Server api') -> SI.Router env)
mapRouter routing f = \_ ctx delayed -> routing Proxy ctx (fmap f delayed)

-- | The `UnmaybeArg` type family is necessary to
-- get `a -> b` from `Maybe a -> b` in a way which doesn't make GHC mad.
-- Used in `HasServer` instances for
-- `WithDefaultApiArg`
type family UnmaybeArg a where
    UnmaybeArg (Maybe a -> b) = a -> b

--
-- Specifying API method parameters with default values
--

-- | `WithDefaultApiArg` type is used to specify API parameters,
-- which are optional but have default values. It can be applied to every
-- API specifier which yields `Maybe a` as argument type.
data WithDefaultApiArg (argType :: * -> *) a

instance ( HasServer (apiType a :> res) ctx
         , Server (apiType a :> res) ~ (Maybe c -> d)
         , Default c
         ) =>
         HasServer (WithDefaultApiArg apiType a :> res) ctx where
    type ServerT (WithDefaultApiArg apiType a :> res) m =
        UnmaybeArg (ServerT (apiType a :> res) m)
    route =
        mapRouter @(apiType a :> res) route $
        \f a -> f $ fromMaybe def a

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

instance ( HasServer (argA a :> argB a :> res) ctx
         , Server (argA a :> argB a :> res) ~ (t b -> t b -> d)
         , Alternative t
         ) =>
         HasServer (AlternativeApiArg argA argB a :> res) ctx where
    type ServerT (AlternativeApiArg argA argB a :> res) m =
        MergeArgs (ServerT (argA a :> argB a :> res) m)
    route =
        mapRouter @(argA a :> argB a :> res) route $
        \f a b -> f $ a <|> b

--
-- Types
--

-- | An empty type which can be used to inject Swagger tags at the type level,
-- directly in the Servant API.
data Tags (tags :: [Symbol])

-- | Specifes logging config for servant.
data WalletLoggingConfig

--
-- Instances
--

-- | Instance of `HasServer` which erases the `Tags` from its routing,
-- as the latter is needed only for Swagger.
instance (HasServer subApi context) => HasServer (Tags tags :> subApi) context where
  type ServerT (Tags tags :> subApi) m = ServerT subApi m
  route _ = route (Proxy @subApi)
  hoistServerWithContext _ = hoistServerWithContext (Proxy @subApi)

instance (HasClient m subApi) => HasClient m (Tags tags :> subApi) where
    type Client m (Tags tags :> subApi) = Client m subApi
    clientWithRoute pm _ = clientWithRoute pm (Proxy @subApi)

-- | Similar to 'instance HasServer', just skips 'Tags'.
instance HasLoggingServer config subApi context =>
         HasLoggingServer config (Tags tags :> subApi) context where
  routeWithLog = mapRouter @(Tags tags :> LoggingApiRec config subApi) route identity

-- If logger config will ever be determined in runtime, 'Data.Reflection.reify'
-- can be used.
-- | Raises 'WalletLoggingConfig' at type level.
instance Reifies WalletLoggingConfig ApiLoggingConfig where
    reflect _ = ApiLoggingConfig ("wallet" <> "api")
