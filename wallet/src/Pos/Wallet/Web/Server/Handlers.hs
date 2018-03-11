{-# LANGUAGE Rank2Types   #-}
{-# LANGUAGE TypeFamilies #-}

-- | Wallet endpoints list

module Pos.Wallet.Web.Server.Handlers
       ( servantHandlers
       , servantHandlersWithSwagger
       ) where

import           Universum

import           Pos.Wallet.Web.Swagger.Spec (swaggerSpecForWalletApi)
import           Servant.API ((:<|>) ((:<|>)))
import           Servant.Server (Handler, Server, hoistServer)
import           Servant.Swagger.UI (swaggerSchemaUIServer)

import           Pos.Core.Txp (TxAux)

import qualified Pos.Wallet.Web.Api as A
import           Pos.Wallet.Web.Mode (MonadFullWalletWebMode)

import           Pos.Wallet.Web.Server.Handlers.Internal (servantHandlers)

----------------------------------------------------------------------------
-- The wallet API with Swagger
----------------------------------------------------------------------------

servantHandlersWithSwagger
    :: MonadFullWalletWebMode ctx m
    => (TxAux -> m Bool)
    -> (forall x. m x -> Handler x)
    -> Server A.WalletSwaggerApi
servantHandlersWithSwagger submitTx nat =
    hoistServer A.walletApi nat (servantHandlers submitTx)
   :<|>
    swaggerSchemaUIServer swaggerSpecForWalletApi
