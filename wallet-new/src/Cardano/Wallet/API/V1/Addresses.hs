
module Cardano.Wallet.API.V1.Addresses where

import           Servant

import           Cardano.Wallet.API.V1.Parameters
import           Cardano.Wallet.API.V1.Types


type API = "addresses" :> WalletRequestParams
                       :> Summary "Returns all the addresses."
                       :> Get '[JSON] (OneOf [Address] (ExtendedResponse [Address]))
      :<|> "addresses" :> ReqBody '[JSON] Address
                       :> Summary "Creates a new Address."
                       :> Post '[JSON] Address
