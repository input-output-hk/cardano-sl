module Cardano.Wallet.API.V1.LegacyHandlers.Wallets where

import           Universum

import qualified Pos.Wallet.Web.ClientTypes.Types as V0
import qualified Pos.Wallet.Web.State.Storage as V0
import qualified Pos.Wallet.Web.Methods as V0
import qualified Pos.Wallet.Web.State as V0 (askWalletSnapshot)

import           Cardano.Wallet.API.Request
import           Cardano.Wallet.API.Response
import qualified Cardano.Wallet.API.V1.LegacyHandlers.Accounts as Accounts
import           Cardano.Wallet.API.V1.Migration
import           Cardano.Wallet.API.V1.Types as V1
import qualified Cardano.Wallet.API.V1.Wallets as Wallets
import qualified Data.IxSet.Typed as IxSet
import           Pos.Update.Configuration ()

import           Pos.Wallet.Web.Methods.Logic (MonadWalletLogic, MonadWalletLogicRead)
import           Servant
import           Test.QuickCheck (arbitrary, generate)

-- | All the @Servant@ handlers for wallet-specific operations.
handlers :: ( HasConfigurations
            , HasCompileInfo
            )
         => ServerT Wallets.API MonadV1
handlers = (newWallet
    :<|> listWallets
    :<|> updatePassword
    :<|> deleteWallet
    :<|> getWallet
    :<|> updateWallet
    ) :<|> Accounts.handlers

-- | Creates a new or restores an existing @wallet@ given a 'NewWallet' payload.
-- Returns to the client the representation of the created or restored
-- wallet in the 'Wallet' type.
newWallet
    :: (MonadThrow m, MonadWalletLogic ctx m)
    => NewWallet
    -> m (WalletResponse Wallet)
newWallet NewWallet{..} = do
    let newWalletHandler CreateWallet  = V0.newWallet
        newWalletHandler RestoreWallet = V0.restoreWallet
        (V1 spendingPassword) = fromMaybe (V1 mempty) newwalSpendingPassword
        (V1 backupPhrase) = newwalBackupPhrase
    initMeta <- V0.CWalletMeta <$> pure newwalName
                              <*> migrate newwalAssuranceLevel
                              <*> pure 0
    let walletInit = V0.CWalletInit initMeta backupPhrase
    single <$> do
        v0wallet <- newWalletHandler newwalOperation spendingPassword walletInit
        addWalletInfo v0wallet

-- | Returns the full (paginated) list of wallets.
listWallets :: (MonadThrow m, V0.MonadWalletLogicRead ctx m)
            => RequestParams
            -> FilterOperations Wallet
            -> SortOperations Wallet
            -> m (WalletResponse [Wallet])
listWallets params fops sops = do
    respondWith params fops sops (IxSet.fromList <$> do
        (V0.getWalletsWithInfo >>= migrate @_ @[V1.Wallet]))

updatePassword
    :: (MonadWalletLogic ctx m)
    => WalletId -> PasswordUpdate -> m (WalletResponse Wallet)
updatePassword wid PasswordUpdate{..} = do
    wid' <- migrate wid
    let (V1 old) = pwdOld
        (V1 new) = pwdNew
    _ <- V0.changeWalletPassphrase wid' old new
    single <$> do
        wallet <- V0.getWallet wid'
        addWalletInfo wallet

-- | Deletes an exisiting wallet.
deleteWallet
    :: (MonadWalletLogic ctx m)
    => WalletId
    -> m NoContent
deleteWallet = V0.deleteWallet <=< migrate

getWallet :: (MonadThrow m, MonadWalletLogicRead ctx m) => WalletId -> m (WalletResponse Wallet)
getWallet wid = do
    wid' <- migrate wid
    wallet <- V0.getWallet wid'
    single <$> addWalletInfo wallet

addWalletInfo :: (MonadThrow m, MonadWalletLogicRead ctx m) => V0.CWallet -> m Wallet
addWalletInfo wallet = do
    mwalletInfo <- V0.getWalletInfo (V0.cwId wallet) <$> V0.askWalletSnapshot
    case mwalletInfo of
        Nothing ->
            error "Throw a 500 or something, this is a bug for sure"
        Just walletInfo ->
            migrate (wallet, walletInfo)

updateWallet
    :: WalletId
    -> WalletUpdate
    -> MonadV1 (WalletResponse Wallet)
updateWallet _ _ = single <$> (liftIO $ generate arbitrary)
