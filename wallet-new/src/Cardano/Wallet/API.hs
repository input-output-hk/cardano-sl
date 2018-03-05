module Cardano.Wallet.API
       ( WalletAPI
       , walletAPI
       , publicAPI
       ) where

import           Servant ((:<|>), (:>), Proxy (..))

import qualified Cardano.Wallet.API.Development as Dev
import qualified Cardano.Wallet.API.V0 as V0
import qualified Cardano.Wallet.API.V1 as V1

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
-- * 'Cardano.Wallet.API.Development.Handlers' contains all the @Handler@s serving the Dev API;

type ExternalAPI =
            "api"         :> V0.API
      :<|>  "api" :> "v1" :> V1.API

type InternalAPI =
            "api" :> "development" :> Dev.API

type WalletAPI = ExternalAPI :<|> InternalAPI

walletAPI :: Proxy WalletAPI
walletAPI = Proxy

publicAPI :: Proxy ExternalAPI
publicAPI = Proxy
