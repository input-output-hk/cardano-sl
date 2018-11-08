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
import           Servant
import qualified Servant.Server.Internal as SI

import           Pos.Util.Servant (ApiLoggingConfig (..), DHeader, DQueryParam,
                     Tags, WithDefaultApiArg)

-- | `mapRouter` is helper function used in order to transform one `HasServer`
-- instance to another. It can be used to introduce custom request params type.
-- See e. g. `WithDefaultApiArg` as an example of usage
mapRouter
    :: forall api api' ctx env.
       (Proxy api -> SI.Context ctx -> SI.Delayed env (Server api) -> SI.Router env)
    -> (Server api' -> Server api)
    -> (Proxy api' -> SI.Context ctx -> SI.Delayed env (Server api') -> SI.Router env)
mapRouter routing f = \_ ctx delayed -> routing Proxy ctx (fmap f delayed)

-- | Specifes logging config for servant.
data WalletLoggingConfig

-- If logger config will ever be determined in runtime, 'Data.Reflection.reify'
-- can be used.
-- | Raises 'WalletLoggingConfig' at type level.
instance Reifies WalletLoggingConfig ApiLoggingConfig where
    reflect _ = ApiLoggingConfig ("wallet" <> "api")
