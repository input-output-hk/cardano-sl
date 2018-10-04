module Cardano.Wallet.API.WIP.Handlers (handlers)
where

import           Universum

import           Servant

import           Cardano.Wallet.API.Response
import           Cardano.Wallet.API.V1.Types as V1
import qualified Cardano.Wallet.API.WIP as WIP (API)
import           Cardano.Wallet.WalletLayer (ActiveWalletLayer (..),
                     PassiveWalletLayer)

-- | WIP @Servant@ handlers the are not part of the offical api yet.
handlers :: ActiveWalletLayer IO -> ServerT WIP.API Handler
handlers awl = checkExternalWallet pwl
           :<|> newExternalWallet pwl
           :<|> deleteExternalWallet pwl
           :<|> newUnsignedTransaction awl
           :<|> newSignedTransaction awl
  where
    pwl = walletPassiveLayer awl

checkExternalWallet :: PassiveWalletLayer IO
                    -> PublicKeyAsBase58
                    -> Handler (WalletResponse WalletAndTxHistory)
checkExternalWallet _pwl _encodedRootPK =
    error "[CHW-54], Cardano Hardware Wallet feature, , check external wallet, unimplemented yet."

newExternalWallet :: PassiveWalletLayer IO
                  -> NewExternalWallet
                  -> Handler (WalletResponse Wallet)
newExternalWallet _pwl _newExtWallet =
    error "[CHW-80], Cardano Hardware Wallet feature, new external wallet, unimplemented yet."

deleteExternalWallet :: PassiveWalletLayer IO
                     -> PublicKeyAsBase58
                     -> Handler NoContent
deleteExternalWallet _pwl _encodedRootPK =
    error "[CHW-106], Cardano Hardware Wallet feature, , delete external wallet, unimplemented yet."

newUnsignedTransaction :: ActiveWalletLayer IO
                       -> PaymentWithChangeAddress
                       -> Handler (WalletResponse RawTransaction)
newUnsignedTransaction _awl _paymentWithChangeAddress =
    error "[CHW-57], Cardano Hardware Wallet, unimplemented yet."

newSignedTransaction :: ActiveWalletLayer IO
                     -> SignedTransaction
                     -> Handler (WalletResponse Transaction)
newSignedTransaction _awl _signedTx =
    error "[CHW-57], Cardano Hardware Wallet, unimplemented yet."
