module Cardano.Wallet.API.V1.Handlers.Wallets where

import           Universum

import qualified Pos.Wallet.Web.ClientTypes.Types as V0
import qualified Pos.Wallet.Web.Methods as V0

import           Cardano.Wallet.API.Request
import           Cardano.Wallet.API.Response
import qualified Cardano.Wallet.API.V1.Handlers.Accounts as Accounts
import           Cardano.Wallet.API.V1.Migration
import           Cardano.Wallet.API.V1.Types as V1
import qualified Cardano.Wallet.API.V1.Wallets as Wallets
import           Pos.Update.Configuration ()

import           Pos.Wallet.Web.Methods.Logic (MonadWalletLogic)
import           Servant
import           Test.QuickCheck (arbitrary, generate)

-- | All the @Servant@ handlers for wallet-specific operations.
handlers :: ( HasConfigurations
            , HasCompileInfo
            )
         => ServerT Wallets.API MonadV1
handlers =   newWallet
        :<|> listWallets
        :<|> (\walletId ->
                     updatePassword walletId
                :<|> deleteWallet walletId
                :<|> getWallet walletId
                :<|> updateWallet walletId
                :<|> Accounts.handlers walletId
             )

-- | Creates a new @wallet@ given a 'NewWallet' payload.
-- Returns to the client the representation of the created
-- wallet in the 'Wallet' type.
newWallet :: (MonadThrow m, MonadWalletLogic ctx m) => NewWallet -> m (WalletResponse Wallet)
newWallet NewWallet{..} = do
  let spendingPassword = fromMaybe mempty newwalSpendingPassword
  initMeta <- V0.CWalletMeta <$> pure newwalName
                             <*> migrate newwalAssuranceLevel
                             <*> pure 0
  let walletInit = V0.CWalletInit initMeta newwalBackupPhrase
  single <$> (V0.newWallet spendingPassword walletInit >>= migrate)

-- | Returns the full (paginated) list of wallets.
listWallets :: (MonadThrow m, V0.MonadWalletLogicRead ctx m)
            => RequestParams
            -> m (WalletResponse [Wallet])
listWallets params = do
    let getWallets = (V0.getWallets >>= migrate @[V0.CWallet] @[V1.Wallet])
    respondWith params (const getWallets)

updatePassword
    :: (MonadWalletLogic ctx m)
    => WalletId -> PasswordUpdate -> m (WalletResponse Wallet)
updatePassword wid PasswordUpdate{..} = do
    wid' <- migrate wid
    _ <- V0.changeWalletPassphrase wid' pwdOld pwdNew
    single <$> (V0.getWallet wid' >>= migrate)

-- | Deletes an exisiting wallet.
deleteWallet
    :: (MonadWalletLogic ctx m)
    => WalletId
    -> m NoContent
deleteWallet = V0.deleteWallet <=< migrate

getWallet
    :: WalletId
    -> MonadV1 (WalletResponse Wallet)
getWallet _ = single <$> (liftIO $ generate arbitrary)

updateWallet
    :: WalletId
    -> WalletUpdate
    -> MonadV1 (WalletResponse Wallet)
updateWallet _ _ = single <$> (liftIO $ generate arbitrary)
