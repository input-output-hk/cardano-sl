{-- Types shared between different API versions. --}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE InstanceSigs    #-}
{-# LANGUAGE RankNTypes      #-}

module Cardano.Wallet.API.Types
    ( DQueryParam
    , DHeader
    , mapRouter
    , WithDefaultApiArg
    , Tags
    , WalletLoggingConfig
    ) where

import           Universum

import           Data.Reflection (Reifies (..))
import           GHC.TypeLits
import           Servant
import qualified Servant.Server.Internal as SI

import           Pos.Util.Servant (ApiLoggingConfig (..), DHeader, DQueryParam,
                     Tags, WithDefaultApiArg)

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
    UnmaybeArg x = TypeError
        ( 'Text "The parameter to UnmaybeArg must be a function with the shape "
        ':$$: 'Text "    Maybe a -> b"
        ':$$: 'Text "You applied it to a type: "
        ':$$: 'Text "    " ':<>: 'ShowType x
        )

--
-- Specifying API method parameters with default values
--

-- | `WithDefaultApiArg` type is used to specify API parameters,
-- which are optional but have default values. It can be applied to every
-- API specifier which yields `Maybe a` as argument type.
-- data WithDefaultApiArg (argType :: * -> *) a
--
-- instance
--     ( HasServer (apiType a :> res) ctx
--     , Server (apiType a :> res) ~ (Maybe c -> d)
--     , UnmaybeArg (Server (apiType a :> res)) ~ (c -> Server res)
--     , Default c
--     )
--     => HasServer (WithDefaultApiArg apiType a :> res) ctx
--   where
--     type ServerT (WithDefaultApiArg apiType a :> res) m =
--         UnmaybeArg (ServerT (apiType a :> res) m)
--     route =
--         mapRouter @(apiType a :> res) route $
--         \f a -> f $ fromMaybe def a
--     hoistServerWithContext
--         :: Proxy (WithDefaultApiArg apiType a :> res)
--         -> Proxy ctx
--         -> (forall x. m x -> n x)
--         -> ServerT (WithDefaultApiArg apiType a :> res) m
--         -- aka
--         -- (UnMaybeArg (ServerT (apiType a :> res) m)
--         -- (UnMaybeArg (Maybe b -> ServerT res m)
--         -- (b -> ServerT res m)
--         -> ServerT (WithDefaultApiArg apiType a :> res) n
--         -- aka
--         -- (UnMaybeArg (ServerT (apiType a :> res) n)
--         -- (UnMaybeArg (Maybe b -> ServerT res n)
--         -- (b -> ServerT res n)
--     hoistServerWithContext _ pc nt s =


-- | Type aliases for query params and headers which have default values.
-- type DQueryParam s a = WithDefaultApiArg (QueryParam s) a
-- type DHeader s a = WithDefaultApiArg (Header s) a

--
-- Types
--

-- | Specifes logging config for servant.
data WalletLoggingConfig

--
-- Instances
--

-- If logger config will ever be determined in runtime, 'Data.Reflection.reify'
-- can be used.
-- | Raises 'WalletLoggingConfig' at type level.
instance Reifies WalletLoggingConfig ApiLoggingConfig where
    reflect _ = ApiLoggingConfig ("wallet" <> "api")
