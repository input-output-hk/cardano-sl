module Cardano.Wallet.API.V1.Handlers.Wallets where

import           Universum

import           Servant

import           Cardano.Wallet.API.Request
import           Cardano.Wallet.API.Response
import           Cardano.Wallet.API.V1.Types as V1
import qualified Cardano.Wallet.API.V1.Wallets as Wallets
import           Cardano.Wallet.WalletLayer (PassiveWalletLayer)
import qualified Cardano.Wallet.WalletLayer as WalletLayer

import           Pos.Core.Common (Coin (..))


-- | All the @Servant@ handlers for wallet-specific operations.
handlers :: PassiveWalletLayer IO -> ServerT Wallets.API Handler
handlers pwl =  newWallet pwl
           :<|> listWallets pwl
           :<|> updatePassword pwl
           :<|> deleteWallet pwl
           :<|> getWallet pwl
           :<|> updateWallet pwl
           :<|> getUtxoStatistics pwl

-- | Creates a new or restores an existing @wallet@ given a 'NewWallet' payload.
-- Returns to the client the representation of the created or restored
-- wallet in the 'Wallet' type.
newWallet :: PassiveWalletLayer IO
          -> NewWallet
          -> Handler (APIResponse Wallet)
newWallet pwl newWalletRequest = do
    -- FIXME(adn) Do not allow creation or restoration of wallets if the underlying node
    -- is still catching up.

    res <- liftIO $ WalletLayer.createWallet pwl (WalletLayer.CreateWallet newWalletRequest)
    case res of
         Left e  -> throwM e
         Right w -> return $ single w

-- | Returns the full (paginated) list of wallets.
listWallets :: PassiveWalletLayer IO
            -> RequestParams
            -> FilterOperations '[WalletId, Coin] Wallet
            -> SortOperations Wallet
            -> Handler (APIResponse [Wallet])
listWallets pwl params fops sops = do
    wallets <- liftIO $ WalletLayer.getWallets pwl
    respondWith params
        fops
        sops
        (pure wallets)

updatePassword :: PassiveWalletLayer IO
               -> WalletId
               -> PasswordUpdate
               -> Handler (APIResponse Wallet)
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
          -> Handler (APIResponse Wallet)
getWallet pwl wid = do
    res <- liftIO $ WalletLayer.getWallet pwl wid
    case res of
         Left e  -> throwM e
         Right w -> return $ single w

updateWallet :: PassiveWalletLayer IO
             -> WalletId
             -> WalletUpdate
             -> Handler (APIResponse Wallet)
updateWallet pwl wid walletUpdateRequest = do
    res <- liftIO $ WalletLayer.updateWallet pwl wid walletUpdateRequest
    case res of
         Left e  -> throwM e
         Right w -> return $ single w

getUtxoStatistics
    :: PassiveWalletLayer IO
    -> WalletId
    -> Handler (APIResponse UtxoStatistics)
getUtxoStatistics pwl wid = do
    res <- liftIO $ WalletLayer.getUtxos pwl wid
    case res of
         Left e  -> throwM e
         Right w ->
            return $ single $ V1.computeUtxoStatistics V1.log10 (map snd w)
