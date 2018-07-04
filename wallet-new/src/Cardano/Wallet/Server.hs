module Cardano.Wallet.Server
    ( walletServer
    , walletDevServer
    , walletDocServer
    , walletDevDocServer
    ) where

import           Servant
import           System.Wlog (Severity)
import           Universum

import           Cardano.Wallet.API
import qualified Cardano.Wallet.API.Development.Handlers as Dev
import qualified Cardano.Wallet.API.V1.Handlers as V1
import           Cardano.Wallet.API.V1.Swagger (swaggerSchemaUIServer)
import qualified Cardano.Wallet.API.V1.Swagger as Swagger
import           Cardano.Wallet.Server.CLI (RunMode (..))
import           Cardano.Wallet.WalletLayer (ActiveWalletLayer)
import           Pos.Update.Configuration (HasUpdateConfiguration,
                     curSoftwareVersion)
import           Pos.Util.CompileInfo (HasCompileInfo, compileInfo)

-- | Serve the REST interface to the wallet
--
-- NOTE: Unlike the legacy server, the handlers will not run in a special
-- Cardano monad because they just interfact with the Wallet object.
walletServer
  :: (Severity -> Text -> IO ()) -- ^ Logging function.
  -> ActiveWalletLayer IO
  -> ServerT WalletAPI IO
walletServer logf awl =
   error "V0 API no longer supported" :<|> V1.handlers logf awl

walletDevServer
  :: (Severity -> Text -> IO ()) -- ^ Logging function.
  -> ActiveWalletLayer IO
  -> RunMode
  -> ServerT WalletDevAPI IO
walletDevServer logf awl runMode =
   Dev.handlers runMode :<|> walletServer logf awl

walletDocServer :: (HasCompileInfo, HasUpdateConfiguration) => Server WalletDocAPI
walletDocServer = v0DocHandler :<|> v1DocHandler
  where
    v0DocHandler = error "V0 API no longer supported"
    v1DocHandler = swaggerSchemaUIServer (Swagger.api (compileInfo, curSoftwareVersion) v1API Swagger.highLevelDescription)

walletDevDocServer :: (HasCompileInfo, HasUpdateConfiguration) => Server WalletDevDocAPI
walletDevDocServer = devDocHandler :<|> walletDocServer
  where
    devDocHandler = swaggerSchemaUIServer (Swagger.api (compileInfo, curSoftwareVersion) devAPI Swagger.highLevelShortDescription)
