
module Cardano.Wallet.API.V1.Addresses where

import           Universum (Text)
import           Servant

import           Cardano.Wallet.API.Response
import           Cardano.Wallet.API.V1.Parameters
import           Cardano.Wallet.API.V1.Types


type API = "addresses" :> WalletRequestParams
                       :> Summary "Returns all the addresses."
                       :> Get '[JSON] (WalletResponse [Address])
      :<|> "addresses" :> ReqBody '[JSON] Address
                       :> Summary "Creates a new Address."
                       :> Post '[JSON] (WalletResponse Address)
      :<|> "addresses" :> Capture "address" Text
                       :> "validity"
                       :> Summary "Checks the validity of an address."
                       :> Get '[JSON] (WalletResponse AddressValidity)
