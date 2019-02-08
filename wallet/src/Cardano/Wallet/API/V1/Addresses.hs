module Cardano.Wallet.API.V1.Addresses where

import           Servant
import           Universum (Text)

import           Cardano.Wallet.API.Response
import           Cardano.Wallet.API.Types
import           Cardano.Wallet.API.V1.Parameters
import           Cardano.Wallet.API.V1.Types

type API = Tag "Addresses" 'NoTagDescription :>
      (    "addresses" :> WalletRequestParams
                       :> Summary "Returns a list of the addresses."
                       :> Get '[ValidJSON] (APIResponse [WalletAddress])
      :<|> "addresses" :> ReqBody '[ValidJSON] NewAddress
                       :> Summary "Creates a new Address."
                       :> Post '[ValidJSON] (APIResponse WalletAddress)
      :<|> "addresses" :> Capture "address" Text
                       :> Summary "Returns interesting information about an address, if available and valid."
                       :> Get '[ValidJSON] (APIResponse WalletAddress)
      :<|> "wallets" :> CaptureWalletId :> "addresses"
        :> Summary "Batch import existing addresses"
        :> ReqBody '[ValidJSON] [V1 Address]
        :> Post '[ValidJSON] (APIResponse (BatchImportResult (V1 Address)))
      )
