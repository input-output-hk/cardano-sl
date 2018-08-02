module Cardano.Wallet.API.V1.Handlers.Wallets where

import           Universum

import           Cardano.Wallet.API.Request
import           Cardano.Wallet.API.Response
import           Cardano.Wallet.API.V1.Types as V1
import qualified Cardano.Wallet.API.V1.Wallets as Wallets

import           Cardano.Wallet.WalletLayer (PassiveWalletLayer (..))
import qualified Cardano.Wallet.WalletLayer.Types as WalletLayer

import           Servant

-- | All the @Servant@ handlers for wallet-specific operations.
handlers :: PassiveWalletLayer IO -> ServerT Wallets.API Handler
handlers pwl =  newWallet pwl
           :<|> listWallets
           :<|> updatePassword pwl
           :<|> deleteWallet pwl
           :<|> getWallet pwl
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
    res <- liftIO $ WalletLayer.createWallet pwl newWalletRequest
    case res of
         Left e  -> throwM e
         Right w -> return $ single w

-- | Returns the full (paginated) list of wallets.
listWallets :: RequestParams
            -> FilterOperations Wallet
            -> SortOperations Wallet
            -> Handler (WalletResponse [Wallet])
listWallets _params _fops _sops = error "Unimplemented. See CBR-227."

updatePassword :: PassiveWalletLayer IO
               -> WalletId
               -> PasswordUpdate
               -> Handler (WalletResponse Wallet)
updatePassword pwl wid passwordUpdate = do
    res <- liftIO $ WalletLayer.updateWalletPassword pwl wid passwordUpdate
    case res of
         Left e  -> throwM e
         Right w -> return $ single w

-- | Deletes an exisiting wallet.
deleteWallet :: PassiveWalletLayer IO
             -> WalletId
             -> Handler NoContent
deleteWallet pwl wid = do
    res <- liftIO $ WalletLayer.deleteWallet pwl wid
    case res of
         Left e   -> throwM e
         Right () -> return NoContent

-- | Gets a specific wallet.
getWallet :: PassiveWalletLayer IO
          -> WalletId
          -> Handler (WalletResponse Wallet)
getWallet pwl wid = do
    res <- liftIO $ WalletLayer.getWallet pwl wid
    case res of
         Left e  -> throwM e
         Right w -> return $ single w

updateWallet :: WalletId
             -> WalletUpdate
             -> Handler (WalletResponse Wallet)
updateWallet _wid _walletUpdate = error "Unimplemented. See CBR-227."
