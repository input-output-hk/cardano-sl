module Cardano.Wallet.API.V1 where


import           Servant ((:<|>), (:>), Get, JSON, Summary)

import           Cardano.Wallet.API.Types
import qualified Cardano.Wallet.API.V1.Addresses as Addresses
import qualified Cardano.Wallet.API.V1.Payments as Payments
import qualified Cardano.Wallet.API.V1.Updates as Updates
import qualified Cardano.Wallet.API.V1.Wallets as Wallets

type API = "version"
           :> Summary "Returns the version for this API."
           :> Get '[JSON] WalletVersion
       :<|> Tags '["Addresses"] :> Addresses.API
       :<|> Tags '["Wallets"]   :> Wallets.API
       :<|> Tags '["Payments"]  :> Payments.API
       :<|> Tags '["Updates"]   :> Updates.API
