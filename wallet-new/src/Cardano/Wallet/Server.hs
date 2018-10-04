module Cardano.Wallet.Server
    ( walletServer
    , walletDevServer
    , walletDocServer
    , walletDevDocServer
    ) where

import           Servant
import           Universum

import           Cardano.Wallet.API
import qualified Cardano.Wallet.API.Development.Handlers as Dev
import qualified Cardano.Wallet.API.V1.Handlers as V1
import           Cardano.Wallet.API.V1.Swagger (swaggerSchemaUIServer)
import qualified Cardano.Wallet.API.V1.Swagger as Swagger
import           Cardano.Wallet.Server.CLI (RunMode (..))
import           Cardano.Wallet.WalletLayer (ActiveWalletLayer)
import           Mockable
import           Pos.Update.Configuration (HasUpdateConfiguration,
                     curSoftwareVersion)
import           Pos.Util.CompileInfo (HasCompileInfo, compileInfo)

-- | Serve the REST interface to the wallet
--
-- NOTE: Unlike the legacy server, the handlers will not run in a special
-- Cardano monad because they just interfact with the Wallet object.
walletServer :: ActiveWalletLayer Production
             -> Server WalletAPI
walletServer w = v0Handler :<|> v1Handler
  where
    -- TODO: Not sure if we want to support the V0 API with the new wallet.
    -- For now I'm assuming we're not going to.
    --
    -- TODO: It'd be nicer to not throw an exception here, but servant doesn't
    -- make this very easy at the moment.
    v0Handler    = error "V0 API no longer supported"
    v1Handler    = V1.handlers w

walletDevServer :: ActiveWalletLayer Production
             -> RunMode
             -> Server WalletDevAPI
walletDevServer w runMode = devHandler :<|> walletHandler
  where
    devHandler    = Dev.handlers runMode
    walletHandler = walletServer w

walletDocServer :: (HasCompileInfo, HasUpdateConfiguration) => Server WalletDocAPI
walletDocServer = v0DocHandler :<|> v1DocHandler
  where
    v0DocHandler = error "V0 API no longer supported"
    v1DocHandler = swaggerSchemaUIServer (Swagger.api (compileInfo, curSoftwareVersion) v1API Swagger.highLevelDescription)

walletDevDocServer :: (HasCompileInfo, HasUpdateConfiguration) => Server WalletDevDocAPI
walletDevDocServer = devDocHandler :<|> walletDocServer
  where
    devDocHandler = swaggerSchemaUIServer (Swagger.api (compileInfo, curSoftwareVersion) devAPI Swagger.highLevelShortDescription)
