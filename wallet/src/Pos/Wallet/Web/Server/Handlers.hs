{-# LANGUAGE Rank2Types   #-}
{-# LANGUAGE TypeFamilies #-}

-- | Wallet endpoints list

module Pos.Wallet.Web.Server.Handlers
       ( servantHandlers
       , servantHandlersWithSwagger
       ) where

import           Universum

import           Pos.Wallet.Web.Swagger.Spec             (swaggerSpecForWalletApi)
import           Servant.API                             ((:<|>) ((:<|>)))
import           Servant.Generic                         (AsServerT, GenericProduct, ToServant,
                                                          toServant)
import           Servant.Server                          (Handler, Server, ServerT, hoistServer)
import           Servant.Swagger.UI                      (swaggerSchemaUIServer)

import           Pos.Core.Txp                            (TxAux)
import           Pos.Update.Configuration                (curSoftwareVersion)

import           Pos.Wallet.WalletMode                   (blockchainSlotDuration)
import           Pos.Wallet.Web.Account                  (GenSeed (RandomSeed))
import qualified Pos.Wallet.Web.Api                      as A
import qualified Pos.Wallet.Web.Methods                  as M
import           Pos.Wallet.Web.Mode                     (MonadFullWalletWebMode)

import           Pos.Wallet.Web.Server.Handlers.Internal

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

----------------------------------------------------------------------------
-- The wallet API
----------------------------------------------------------------------------

servantHandlers :: MonadFullWalletWebMode ctx m => (TxAux -> m Bool) -> ServerT A.WalletApi m
servantHandlers submitTx = toServant' A.WalletApiRecord
    { _test        = testHandlers
    , _wallets     = walletsHandlers
    , _accounts    = accountsHandlers
    , _addresses   = addressesHandlers
    , _profile     = profileHandlers
    , _txs         = txsHandlers submitTx
    , _update      = updateHandlers
    , _redemptions = redemptionsHandlers submitTx
    , _reporting   = reportingHandlers
    , _settings    = settingsHandlers
    , _backup      = backupHandlers
    , _info        = infoHandlers
    , _system      = systemHandlers
    }

