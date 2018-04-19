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
import           Cardano.Wallet.WalletLayer (ActiveWalletLayer)
import           Cardano.Wallet.Server.CLI (RunMode (..))
import           Pos.Update.Configuration (HasUpdateConfiguration, curSoftwareVersion)
import           Pos.Util.CompileInfo (HasCompileInfo, compileInfo)
import           Servant.Swagger.UI (swaggerSchemaUIServer)

-- | Serve the REST interface to the wallet
--
-- NOTE: Unlike the legacy server, the handlers will not run in a special
-- Cardano monad because they just interfact with the Wallet object.
walletServer :: forall m. (HasCompileInfo, HasUpdateConfiguration)
             => ActiveWalletLayer m
             -> Server WalletAPI
walletServer w = v0DocHandler :<|> v1DocHandler :<|> v0Handler :<|> v1Handler
  where
    -- TODO: Not sure if we want to support the V0 API with the new wallet.
    -- For now I'm assuming we're not going to.
    --
    -- TODO: It'd be nicer to not throw an exception here, but servant doesn't
    -- make this very easy at the moment.
    v0DocHandler = error "V0 API no longer supported"
    v0Handler    = v0DocHandler
    v1DocHandler = swaggerSchemaUIServer (Swagger.api (compileInfo, curSoftwareVersion) v1API Swagger.highLevelDescription)
    v1Handler    = V1.handlers w


walletDevServer :: forall m. (HasCompileInfo, HasUpdateConfiguration)
                => ActiveWalletLayer m
                -> RunMode
                -> Server WalletDevAPI
walletDevServer w runMode = devDocHandler :<|> devHandler :<|> walletHandler
  where
    devDocHandler = swaggerSchemaUIServer (Swagger.api (compileInfo, curSoftwareVersion) devAPI Swagger.highLevelShortDescription)
    devHandler    = Dev.handlers runMode
    walletHandler = walletServer w
