module Cardano.Wallet.Server
    ( walletServer
    , walletDocServer
    ) where

import           Universum

import           Servant

import           Pos.Chain.Update (HasUpdateConfiguration, curSoftwareVersion)
import           Pos.Util.CompileInfo (HasCompileInfo, compileInfo)

import           Cardano.Wallet.API
import qualified Cardano.Wallet.API.Internal.Handlers as Internal
import qualified Cardano.Wallet.API.V1.Handlers as V1
import           Cardano.Wallet.API.V1.Swagger (swaggerSchemaUIServer)
import qualified Cardano.Wallet.API.V1.Swagger as Swagger
import qualified Cardano.Wallet.API.WIP.Handlers as WIP (handlers)
import           Cardano.Wallet.Server.CLI (RunMode (..))
import           Cardano.Wallet.WalletLayer (ActiveWalletLayer (..))

-- | Serve the REST interface to the wallet
--
-- NOTE: Unlike the legacy server, the handlers will not run in a special
-- Cardano monad because they just interfact with the Wallet object.
walletServer :: ActiveWalletLayer IO
             -> RunMode
             -> Server WalletAPI
walletServer w _ =
    v1Handler
    :<|> internalHandler
    :<|> wipHandler
  where
    v1Handler       = V1.handlers w
    internalHandler = Internal.handlers (walletPassiveLayer w)
    wipHandler      = WIP.handlers w

walletDocServer :: (HasCompileInfo, HasUpdateConfiguration) => Server WalletDoc
walletDocServer =
    v1DocHandler
  where
    infos        = (compileInfo, curSoftwareVersion)
    v1DocHandler = swaggerSchemaUIServer
        (Swagger.api infos walletDocAPI Swagger.highLevelDescription)
