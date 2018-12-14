-- | This module contains the top level API definition for the Cardano hardware wallet.
-- The Cardano hardware wallet is work in progress.
--
module Cardano.Wallet.API.WIP where

import           Servant

import           Cardano.Wallet.API.Response
import           Cardano.Wallet.API.Types
import           Cardano.Wallet.API.V1.Types

type API = Tags '["WIP"] :>
    (
         "external-wallets"
                   :> Summary "Creates a new externally-owned sequential (EOS) wallet."
                   :> ReqBody '[ValidJSON] (New EosWallet)
                   :> PostCreated '[ValidJSON] (APIResponse EosWallet)
    :<|> "external-wallets"
                   :> Capture "eosWalletId" EosWalletId
                   :> Summary "Deletes the given externally-owned sequential (EOS) wallet and all its accounts."
                   :> DeleteNoContent '[ValidJSON] NoContent
    :<|> "external-transactions" :> "unsigned"
                   :> Summary "Creates a new unsigned transaction (it will be signed externally)."
                   :> ReqBody '[ValidJSON] Payment
                   :> Post '[ValidJSON] (APIResponse UnsignedTransaction)
    :<|> "external-transactions"
                   :> Summary "Publish an externally-signed transaction."
                   :> ReqBody '[ValidJSON] SignedTransaction
                   :> Post '[ValidJSON] (APIResponse Transaction)
    )
