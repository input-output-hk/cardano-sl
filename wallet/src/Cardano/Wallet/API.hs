module Cardano.Wallet.API
       ( -- * Wallet API Top-Level Representations
         WalletAPI
       , walletAPI
       , WalletDoc
       , walletDoc
       , walletDocAPI

         -- * Components Representations
       , V1API
       , v1API
       , InternalAPI
       , internalAPI
       , WIPAPI
       , wipAPI
       ) where

import           Cardano.Wallet.API.Types (WalletLoggingConfig)
import           Pos.Util.Servant (LoggingApi)
import           Servant ((:<|>), (:>), Proxy (..))
import           Servant.Swagger.UI (SwaggerSchemaUI)

import qualified Cardano.Wallet.API.Internal as Internal
import qualified Cardano.Wallet.API.V1 as V1
import qualified Cardano.Wallet.API.WIP as WIP

-- | The complete API, qualified by its versions. For backward compatibility's
-- sake, we still expose the old API under @/api/@. Specification is split under
-- separate modules.
--
-- Unsurprisingly:
--
-- * 'Cardano.Wallet.API.V1' hosts the full specification of the V1 API;
--
-- This project uses Servant, which means the logic is separated from the
-- implementation (i.e. the Server). Such server, together with all its web
-- handlers lives in an executable which contains the aptly-named modules:
--
-- * 'Cardano.Wallet.Server' contains the main server;
-- * 'Cardano.Wallet.API.V1.Handlers' contains all the @Handler@s serving the V1 API;
-- * 'Cardano.Wallet.API.Internal.Handlers' contains all the @Handler@s serving the Internal API;
-- * 'Cardano.Wallet.API.WIP.Handlers' contains @Handler@s still being worked on;

type WalletAPI = LoggingApi WalletLoggingConfig (V1API :<|> InternalAPI :<|> WIPAPI)
walletAPI :: Proxy WalletAPI
walletAPI = Proxy

type WalletDoc = "docs" :> "v1" :> SwaggerSchemaUI "index" "swagger.json"
walletDoc :: Proxy WalletDoc
walletDoc = Proxy
walletDocAPI :: Proxy (V1API :<|> InternalAPI)
walletDocAPI = Proxy


type V1API = "api" :> "v1" :> V1.API
v1API :: Proxy V1API
v1API = Proxy

type InternalAPI = "api" :> "internal" :> Internal.API
internalAPI :: Proxy InternalAPI
internalAPI = Proxy

type WIPAPI = "api" :> "unimplemented" :> WIP.API
wipAPI :: Proxy WIPAPI
wipAPI = Proxy
