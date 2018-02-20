
module Cardano.Wallet.API
       ( WalletAPI
       , WalletSwaggerApi
       , swaggerWalletApi
       , walletAPI
       ) where

import           Servant ((:<|>), (:>), Proxy (..))

import           Cardano.Wallet.API.Types
import qualified Cardano.Wallet.API.V0 as V0
import qualified Cardano.Wallet.API.V1 as V1
import qualified Cardano.Wallet.API.Dev as Dev

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
-- * 'Cardano.Wallet.API.Dev.Handlers' contains all the @Handler@s serving the Dev API;
--
-- type WalletAPI
--     =     WalletPublicAPI
--     :<|>  WalletDevAPI

type V0_API
     = "api" :> Tags '["V0 (Deprecated)"]
             :> V0.API

type V1_API
     = "api" :> "v1"
             :> Tags '["V1"]
             :> V1.API

type DEV_API
     = "api" :> "development"
             :> Tags '["Development"]
             :> Dev.API

type WalletAPI
    =     V0_API
    :<|>  V1_API
    :<|>  DEV_API

type WalletSwaggerApi
    =     V0_API
    :<|>  V1_API

walletAPI :: Proxy WalletAPI
walletAPI = Proxy

swaggerWalletApi :: Proxy WalletSwaggerApi
swaggerWalletApi = Proxy
