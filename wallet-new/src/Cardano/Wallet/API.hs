module Cardano.Wallet.API
       ( V0API
       , V0API'
       , v0API
       , V1API
       , v1API
       , InternalAPI
       , internalAPI
       , WalletAPI
       , walletAPI
       , WalletDocAPI
       , walletDocAPI
       , WIPAPI
       , NewWalletDocAPI
       , newWalletDocAPI
       , NewWalletAPI
       , newWalletAPI
       ) where

import           Cardano.Wallet.API.Types (WalletLoggingConfig)
import           Pos.Util.Servant (LoggingApi)
import           Servant ((:<|>), (:>), Proxy (..))
import           Servant.Swagger.UI (SwaggerSchemaUI)

import qualified Cardano.Wallet.API.Internal as Internal
import qualified Cardano.Wallet.API.V0 as V0
import qualified Cardano.Wallet.API.V1 as V1
import qualified Cardano.Wallet.API.WIP as WIP

-- | The complete API, qualified by its versions. For backward compatibility's sake, we still expose
-- the old API under @/api/@. Specification is split under separate modules.
-- Unsurprisingly:
--
-- * 'Cardano.Wallet.API.V0' hosts the full specification of the V0 (Legacy) API;
-- * 'Cardano.Wallet.API.V1' hosts the full specification of the V1 API;
--
-- This project uses Servant, which means the logic is separated from the implementation (i.e. the Server).
-- Such server, together with all its web handlers lives in an executable which contains the aptly-named
-- modules:
--
-- * 'Cardano.Wallet.Server' contains the main server;
-- * 'Cardano.Wallet.API.V0.Handlers' contains all the @Handler@s serving the V0 API;
-- * 'Cardano.Wallet.API.V1.Handlers' contains all the @Handler@s serving the V1 API;
-- * 'Cardano.Wallet.API.Internalelopment.Handlers' contains all the @Handler@s serving the Internal API;

type V0Doc = "docs" :> "v0" :> SwaggerSchemaUI "index" "swagger.json"
type V1Doc = "docs" :> "v1" :> SwaggerSchemaUI "index" "swagger.json"


type V0API  = "api" :> V0.API
type V0API' = "api" :> "v0" :> V0.API
v0API :: Proxy V0API
v0API = Proxy

type V1API = "api" :> "v1" :> V1.API
v1API :: Proxy V1API
v1API = Proxy

type InternalAPI = "api" :> "internal" :> Internal.API
internalAPI :: Proxy InternalAPI
internalAPI = Proxy

type WIPAPI = "api" :> "unimplemented" :> WIP.API

type WalletAPI = LoggingApi WalletLoggingConfig (V0API' :<|> V0API :<|> V1API :<|> InternalAPI :<|> WIPAPI)
walletAPI :: Proxy WalletAPI
walletAPI = Proxy

type WalletDocAPI = V0Doc :<|> V1Doc
walletDocAPI :: Proxy WalletDocAPI
walletDocAPI = Proxy

type NewWalletAPI = LoggingApi WalletLoggingConfig (V1API :<|> InternalAPI :<|> WIPAPI)
newWalletAPI :: Proxy NewWalletAPI
newWalletAPI = Proxy

type NewWalletDocAPI = V1Doc
newWalletDocAPI :: Proxy NewWalletDocAPI
newWalletDocAPI = Proxy
