module Cardano.Wallet.API.V1.LegacyHandlers.Wallets where

import           Universum
import           UnliftIO (MonadUnliftIO)

import qualified Pos.Wallet.Web.ClientTypes.Types as V0
import qualified Pos.Wallet.Web.Methods as V0

import           Cardano.Wallet.API.Request
import           Cardano.Wallet.API.Response
import qualified Cardano.Wallet.API.V1.LegacyHandlers.Accounts as Accounts
import           Cardano.Wallet.API.V1.Migration
import           Cardano.Wallet.API.V1.Types as V1
import qualified Cardano.Wallet.API.V1.Wallets as Wallets
import qualified Data.IxSet.Typed as IxSet
import           Pos.Update.Configuration ()

import           Pos.Util (HasLens (..))
import           Pos.Wallet.Web.Methods.Logic (MonadWalletLogic, MonadWalletLogicRead)
import           Pos.Wallet.Web.Tracking.Types (SyncQueue)
import           Servant
import           Test.QuickCheck (arbitrary, generate)

-- | All the @Servant@ handlers for wallet-specific operations.
handlers :: ( HasConfigurations
            , HasCompileInfo
            )
         => ServerT Wallets.API MonadV1
handlers =
        (    newWallet
        :<|> listWallets
        :<|> (\walletId ->
                     updatePassword walletId
                :<|> deleteWallet walletId
                :<|> getWallet walletId
                :<|> updateWallet walletId
             )
        )
        -- NOTE Separation between tags ("Wallets" vs "Accounts"), see Cardano.Wallet.API.V1.Wallets
        :<|> (\walletId -> Accounts.handlers walletId)

-- | Creates a new or restores an existing @wallet@ given a 'NewWallet' payload.
-- Returns to the client the representation of the created or restored
-- wallet in the 'Wallet' type.
newWallet :: ( MonadThrow m
             , MonadUnliftIO m
             , MonadWalletLogic ctx m
             , HasLens SyncQueue ctx SyncQueue
             ) => NewWallet -> m (WalletResponse Wallet)
newWallet NewWallet{..} = do
  let newWalletHandler CreateWallet  = V0.newWalletHandler
      newWalletHandler RestoreWallet = V0.restoreWalletFromSeed
      (V1 spendingPassword) = fromMaybe (V1 mempty) newwalSpendingPassword
      (V1 backupPhrase) = newwalBackupPhrase
  initMeta <- V0.CWalletMeta <$> pure newwalName
                             <*> migrate newwalAssuranceLevel
                             <*> pure 0
  let walletInit = V0.CWalletInit initMeta backupPhrase
  single <$> (newWalletHandler newwalOperation spendingPassword walletInit >>= migrate)

-- | Returns the full (paginated) list of wallets.
listWallets :: (MonadThrow m, V0.MonadWalletLogicRead ctx m)
            => RequestParams
            -> FilterOperations Wallet
            -> SortOperations Wallet
            -> m (WalletResponse [Wallet])
listWallets params fops sops = do
    respondWith params fops sops (IxSet.fromList <$> (V0.getWallets >>= migrate @[V0.CWallet] @[V1.Wallet]))

updatePassword
    :: (MonadWalletLogic ctx m)
    => WalletId -> PasswordUpdate -> m (WalletResponse Wallet)
updatePassword wid PasswordUpdate{..} = do
    wid' <- migrate wid
    let (V1 old) = pwdOld
        (V1 new) = pwdNew
    _ <- V0.changeWalletPassphrase wid' old new
    single <$> (V0.getWallet wid' >>= migrate)

-- | Deletes an exisiting wallet.
deleteWallet
    :: (MonadWalletLogic ctx m)
    => WalletId
    -> m NoContent
deleteWallet = V0.deleteWallet <=< migrate

getWallet :: (MonadThrow m, MonadWalletLogicRead ctx m) => WalletId -> m (WalletResponse Wallet)
getWallet = fmap single . migrate <=< V0.getWallet <=< migrate

updateWallet
    :: WalletId
    -> WalletUpdate
    -> MonadV1 (WalletResponse Wallet)
updateWallet _ _ = single <$> (liftIO $ generate arbitrary)
