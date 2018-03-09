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
walletServer :: (Migration.HasConfigurations, Migration.HasCompileInfo)
             => (forall a. WalletWebMode a -> Handler a)
             -> Diffusion WalletWebMode
             -> Server WalletAPI
walletServer natV0 diffusion = v0DocHdl :<|> v1DocHdl :<|> v0Hdl :<|> v1Hdl
  where
    v0DocHdl = swaggerSchemaUIServer (Swagger.api (compileInfo, curSoftwareVersion) v0API Swagger.highLevelShortDescription)
    v0Hdl    = V0.handlers natV0 diffusion
    v1DocHdl = swaggerSchemaUIServer (Swagger.api (compileInfo, curSoftwareVersion) v1API Swagger.highLevelDescription)
    v1Hdl    = V1.handlers natV0 diffusion


walletDevServer :: (Migration.HasConfigurations, Migration.HasCompileInfo)
             => (forall a. WalletWebMode a -> Handler a)
             -> Diffusion WalletWebMode
             -> RunMode
             -> Server WalletDevAPI
walletDevServer natV0 diffusion runMode = devDocHdl :<|> devHdl :<|> walletHdl
  where
    devDocHdl = swaggerSchemaUIServer (Swagger.api (compileInfo, curSoftwareVersion) devAPI Swagger.highLevelShortDescription)
    devHdl    = Dev.handlers natV0 runMode
    walletHdl = walletServer natV0 diffusion
