{-- Types shared between different API versions. --}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
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
import           Data.Default (Default (..))
import qualified Data.Text as T
import           GHC.TypeLits
import qualified Servant.Server.Internal as SI

import           Servant
import           Servant.API.Sub ((:>))
import           Test.QuickCheck

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
