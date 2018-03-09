module Cardano.Wallet.Server
    ( walletServer
    , walletDevServer
    ) where

import           Servant
import           Universum

import           Cardano.Wallet.API
import qualified Cardano.Wallet.API.Development.Handlers as Dev
import qualified Cardano.Wallet.API.V1.Handlers as V1
import qualified Cardano.Wallet.API.V1.Swagger as Swagger
import           Cardano.Wallet.Kernel
import           Cardano.Wallet.Server.CLI (RunMode (..))
import           Pos.Update.Configuration (HasUpdateConfiguration, curSoftwareVersion)
import           Pos.Util.CompileInfo (HasCompileInfo, compileInfo)
import           Servant.Swagger.UI (swaggerSchemaUIServer)

-- | Serve the REST interface to the wallet
--
-- NOTE: Unlike the legacy server, the handlers will not run in a special
-- Cardano monad because they just interfact with the Wallet object.
walletServer :: (HasCompileInfo, HasUpdateConfiguration)
             => ActiveWallet
             -> Server WalletAPI
walletServer w = v0DocHdl :<|> v1DocHdl :<|> v0Hdl :<|> v1Hdl
  where
    -- TODO: Not sure if we want to support the V0 API with the new wallet.
    -- For now I'm assuming we're not going to.
    --
    -- TODO: It'd be nicer to not throw an exception here, but servant doesn't
    -- make this very easy at the moment.
    v0DocHdl = error "V0 API no longer supported"
    v0Hdl    = v0DocHdl
    v1DocHdl = swaggerSchemaUIServer (Swagger.api (compileInfo, curSoftwareVersion) v1API Swagger.highLevelDescription)
    v1Hdl    = V1.handlers w


walletDevServer :: (HasCompileInfo, HasUpdateConfiguration)
             => ActiveWallet
             -> RunMode
             -> Server WalletDevAPI
walletDevServer w runMode = devDocHdl :<|> devHdl :<|> walletHdl
  where
    devDocHdl = swaggerSchemaUIServer (Swagger.api (compileInfo, curSoftwareVersion) devAPI Swagger.highLevelShortDescription)
    devHdl    = Dev.handlers runMode
    walletHdl = walletServer w
