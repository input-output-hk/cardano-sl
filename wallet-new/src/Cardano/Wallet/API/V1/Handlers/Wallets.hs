module Cardano.Wallet.API.V1.Handlers.Wallets where

import           Universum

import           Cardano.Wallet.API.Request
import           Cardano.Wallet.API.Response
import           Cardano.Wallet.API.V1.Types as V1
import qualified Cardano.Wallet.API.V1.Wallets as Wallets

import           Cardano.Wallet.WalletLayer (PassiveWalletLayer (..))

import           Servant

-- | All the @Servant@ handlers for wallet-specific operations.
handlers :: PassiveWalletLayer IO -> ServerT Wallets.API Handler
handlers pwl =  newWallet pwl
           :<|> listWallets
           :<|> updatePassword
           :<|> deleteWallet
           :<|> getWallet
           :<|> updateWallet


-- | Creates a new or restores an existing @wallet@ given a 'NewWallet' payload.
-- Returns to the client the representation of the created or restored
-- wallet in the 'Wallet' type.
newWallet :: PassiveWalletLayer IO
          -> NewWallet
          -> Handler (WalletResponse Wallet)
newWallet pwl newWalletRequest = do
    -- FIXME(adn) Do not allow creation or restoration of wallets if the underlying node
    -- is still catching up.

    -- FIXME(adn) Wallet restoration from seed will be provided as part of
    -- CBR-243.
    res <- liftIO $ (_pwlCreateWallet pwl) newWalletRequest
    case res of
         Left e  -> throwM e
         Right w -> return $ single w

-- | Returns the full (paginated) list of wallets.
listWallets :: RequestParams
            -> FilterOperations Wallet
            -> SortOperations Wallet
            -> Handler (WalletResponse [Wallet])
listWallets _params _fops _sops = error "Unimplemented. See CBR-227."

updatePassword :: WalletId
               -> PasswordUpdate
               -> Handler (WalletResponse Wallet)
updatePassword _wid _passwordUpdate = error "Unimplemented. See CBR-227."

-- | Deletes an exisiting wallet.
deleteWallet :: WalletId -> Handler NoContent
deleteWallet _wid = error "Unimplemented. See CBR-227."

getWallet :: WalletId -> Handler (WalletResponse Wallet)
getWallet _wid = error "Unimplemented. See CBR-227."

updateWallet :: WalletId
             -> WalletUpdate
             -> Handler (WalletResponse Wallet)
updateWallet _wid _walletUpdate = error "Unimplemented. See CBR-227."
