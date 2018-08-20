module Cardano.Wallet.API.V1.Handlers.Wallets where

import           Universum

import           Cardano.Wallet.API.Request
import           Cardano.Wallet.API.Response
import           Cardano.Wallet.API.V1.Types as V1
import qualified Cardano.Wallet.API.V1.Wallets as Wallets
import           Cardano.Wallet.WalletLayer (PassiveWalletLayer)
import qualified Cardano.Wallet.WalletLayer as WalletLayer

import           Pos.Chain.Txp (Utxo)
import           Pos.Core.Common (Coin (..))
import           Pos.Core.Txp (TxOut (..), TxOutAux (..))

import qualified Data.Map.Strict as M (elems)
import           Servant

-- | All the @Servant@ handlers for wallet-specific operations.
handlers :: PassiveWalletLayer IO -> ServerT Wallets.API Handler
handlers pwl =  newWallet pwl
           :<|> listWallets pwl
           :<|> updatePassword pwl
           :<|> deleteWallet pwl
           :<|> getWallet pwl
           :<|> updateWallet pwl
           :<|> getUtxoStatistics pwl
           :<|> checkExternalWallet pwl
           :<|> newExternalWallet pwl
           :<|> deleteExternalWallet pwl

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
listWallets :: PassiveWalletLayer IO
            -> RequestParams
            -> FilterOperations '[WalletId, Coin] Wallet
            -> SortOperations Wallet
            -> Handler (WalletResponse [Wallet])
listWallets pwl params fops sops = do
    wallets <- liftIO $ WalletLayer.getWallets pwl
    respondWith params
        fops
        sops
        (pure wallets)

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

updateWallet :: PassiveWalletLayer IO
             -> WalletId
             -> WalletUpdate
             -> Handler (WalletResponse Wallet)
updateWallet pwl wid walletUpdateRequest = do
    res <- liftIO $ WalletLayer.updateWallet pwl wid walletUpdateRequest
    case res of
         Left e  -> throwM e
         Right w -> return $ single w

getUtxoStatistics
    :: PassiveWalletLayer IO
    -> WalletId
    -> Handler (WalletResponse UtxoStatistics)
getUtxoStatistics pwl wid = do
    res <- liftIO $ WalletLayer.getUtxos pwl wid
    case res of
         Left e  -> throwM e
         Right w -> do
             let extractValue :: TxOutAux ->  Word64
                 extractValue = getCoin . txOutValue . toaOut
             let utxosCoinValuesForAllAccounts :: [(Account, Utxo)] -> [Word64]
                 utxosCoinValuesForAllAccounts =
                     concatMap (\pair -> map extractValue (M.elems $ snd pair) )
             return $ single (V1.computeUtxoStatistics V1.log10 $ utxosCoinValuesForAllAccounts w)

checkExternalWallet :: PassiveWalletLayer IO
                    -> PublicKeyAsBase58
                    -> Handler (WalletResponse WalletAndTxHistory)
checkExternalWallet _encodedRootPK =
    error "[CHW-54], Cardano Hardware Wallet feature, , check external wallet, unimplemented yet."

newExternalWallet :: PassiveWalletLayer IO
                  -> NewExternalWallet
                  -> Handler (WalletResponse Wallet)
newExternalWallet _newExtWallet =
    error "[CHW-80], Cardano Hardware Wallet feature, new external wallet, unimplemented yet."

deleteExternalWallet :: PassiveWalletLayer IO
                     -> PublicKeyAsBase58
                     -> Handler NoContent
deleteExternalWallet _encodedRootPK =
    error "[CHW-106], Cardano Hardware Wallet feature, , delete external wallet, unimplemented yet."
