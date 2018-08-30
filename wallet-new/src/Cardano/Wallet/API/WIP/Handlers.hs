module Cardano.Wallet.API.WIP.Handlers (handlers)
where

import           Universum

import           Servant

import           Cardano.Wallet.API.Response
import           Cardano.Wallet.API.V1.Types as V1
import qualified Cardano.Wallet.API.WIP as WIP (API)
import           Cardano.Wallet.WalletLayer (PassiveWalletLayer)

-- | WIP @Servant@ handlers the are not part of the offical api yet.
handlers :: PassiveWalletLayer IO -> ServerT WIP.API Handler
handlers pwl = checkExternalWallet pwl
           :<|> newExternalWallet pwl
           :<|> deleteExternalWallet pwl

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
