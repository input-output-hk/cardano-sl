{-# LANGUAGE RankNTypes #-}
module Cardano.Wallet.LegacyServer where

import           Cardano.Wallet.API
import           Cardano.Wallet.API.V1.Migration as Migration

import qualified Cardano.Wallet.API.Development.LegacyHandlers as Dev
import qualified Cardano.Wallet.API.V0.Handlers as V0
import qualified Cardano.Wallet.API.V1.LegacyHandlers as V1
import qualified Cardano.Wallet.API.V1.Swagger as Swagger
import           Cardano.Wallet.Server.CLI (RunMode (..))

import           Pos.Diffusion.Types (Diffusion (..))
import           Pos.Update.Configuration (curSoftwareVersion)
import           Pos.Util.CompileInfo (compileInfo)
import           Pos.Wallet.Web.Mode (WalletWebMode)
import           Servant
import           Servant.Swagger.UI (swaggerSchemaUIServer)

-- | This function has the tricky task of plumbing different versions of the API,
-- with potentially different monadic stacks into a uniform @Server@ we can use
-- with Servant.
walletServer :: ( Migration.HasConfigurations
                , Migration.HasCompileInfo
                )
             => (forall a. WalletWebMode a -> Handler a)
             -> Diffusion WalletWebMode
             -> RunMode
             -> Server WalletAPI
walletServer natV0 diffusion runMode = externalAPI :<|> internalAPI
  where
    v0Doc       = swaggerSchemaUIServer (Swagger.api (compileInfo, curSoftwareVersion) v0API)
    v0Handlers  = V0.handlers natV0 diffusion
    v1Doc       = swaggerSchemaUIServer (Swagger.api (compileInfo, curSoftwareVersion) v1API)
    v1Handlers  = V1.handlers natV0 diffusion
    externalAPI = v0Doc :<|> v1Doc :<|> v0Handlers :<|> v1Handlers
    internalAPI = Dev.handlers natV0 runMode
