module Cardano.Wallet.Server
    ( walletServer
    , walletDocServer
    ) where

import           Universum

import           Servant

import           Cardano.Wallet.API
import           Cardano.Wallet.API.V1.Swagger (swaggerSchemaUIServer)
import           Cardano.Wallet.Server.CLI (RunMode (..))
import           Cardano.Wallet.WalletLayer (ActiveWalletLayer)
import           Pos.Chain.Update (HasUpdateConfiguration, curSoftwareVersion)
import           Pos.Util.CompileInfo (HasCompileInfo, compileInfo)

import qualified Cardano.Wallet.API.V1.Handlers as V1
import qualified Cardano.Wallet.API.V1.Swagger as Swagger


-- | Serve the REST interface to the wallet
--
-- NOTE: Unlike the legacy server, the handlers will not run in a special
-- Cardano monad because they just interfact with the Wallet object.
walletServer :: ActiveWalletLayer IO
             -> RunMode
             -> Server WalletAPI
walletServer w _ =
         v0Handler
    :<|> v0Handler
    :<|> v1Handler
    :<|> internalHandler
  where
    -- TODO: It'd be nicer to not throw an exception here, but servant doesn't
    -- make this very easy at the moment.
    v0Handler       = error "V0 API no longer supported"
    v1Handler       = V1.handlers w
    internalHandler = error "TODO: Internal API not yet defined (CBR-377)"


walletDocServer :: (HasCompileInfo, HasUpdateConfiguration) => Server WalletDocAPI
walletDocServer = v0DocHandler :<|> v1DocHandler
  where
    v0DocHandler = error "V0 API no longer supported"
    v1DocHandler = swaggerSchemaUIServer (Swagger.api (compileInfo, curSoftwareVersion) v1API Swagger.highLevelDescription)
