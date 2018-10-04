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
                   :> Capture "rootPK" PublicKeyAsBase58
                   :> Summary "Check if this external wallet is presented in the node."
                   :> PostCreated '[ValidJSON] (WalletResponse WalletAndTxHistory)
    :<|> "external-wallets"
                   :> Summary "Creates a new or restores an existing external wallet (mobile client or hardware wallet)."
                   :> ReqBody '[ValidJSON] (New ExternalWallet)
                   :> PostCreated '[ValidJSON] (WalletResponse Wallet)
    :<|> "external-wallets"
                   :> Capture "rootPK" PublicKeyAsBase58
                   :> Summary "Deletes the given external wallet and all its accounts."
                   :> DeleteNoContent '[ValidJSON] NoContent
    :<|> "external-transactions" :> "unsigned"
                   :> Summary "Creates a new unsigned transaction (it will be signed externally)."
                   :> ReqBody '[ValidJSON] PaymentWithChangeAddress
                   :> Post '[ValidJSON] (WalletResponse RawTransaction)
    :<|> "external-transactions"
                   :> Summary "Publish an externally-signed transaction."
                   :> ReqBody '[ValidJSON] SignedTransaction
                   :> Post '[ValidJSON] (WalletResponse Transaction)
    )
