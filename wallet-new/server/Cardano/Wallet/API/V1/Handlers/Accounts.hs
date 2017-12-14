module Cardano.Wallet.API.V1.Handlers.Accounts (
      handlers
    ) where

import           Universum

import           Cardano.Wallet.API.Request
import           Cardano.Wallet.API.Response
import qualified Cardano.Wallet.API.V1.Accounts as Accounts
import           Cardano.Wallet.API.V1.Migration
import           Cardano.Wallet.API.V1.Types
import qualified Data.IxSet.Typed as IxSet

import qualified Pos.Wallet.Web.Account as V0
import qualified Pos.Wallet.Web.ClientTypes.Types as V0
import qualified Pos.Wallet.Web.Methods.Logic as V0
import qualified Pos.Wallet.Web.Account as V0
import qualified Pos.Wallet.Web.Tracking as V0
import           Servant

handlers
    :: (HasCompileInfo, HasConfigurations)
    => WalletId -> ServerT Accounts.API MonadV1
handlers walletId =
          deleteAccount walletId
    :<|>  getAccount walletId
    :<|>  listAccounts walletId
    :<|>  newAccount walletId
    :<|>  updateAccount walletId

deleteAccount
    :: (V0.MonadWalletLogic ctx m)
    => WalletId -> AccountId -> m NoContent
deleteAccount wId accId =
    migrate (wId, accId) >>= V0.deleteAccount

getAccount
    :: (MonadThrow m, V0.MonadWalletLogicRead ctx m)
    => WalletId -> AccountId -> m (WalletResponse Account)
getAccount wId accId =
    single <$> (migrate (wId, accId) >>= V0.getAccount >>= migrate)

listAccounts
    :: (MonadThrow m, V0.MonadWalletLogicRead ctx m)
    => WalletId -> RequestParams -> m (WalletResponse [Account])
listAccounts wId params =
    let accounts = migrate wId >>= V0.getAccounts . Just >>= migrate @[V0.CAccount] @[Account]
    in respondWith params (NoFilters :: FilterOperations Account)
                          (NoSorts :: SortOperations Account)
                          (IxSet.fromList <$> accounts)

newAccount
    :: (V0.MonadWalletLogic ctx m)
    => WalletId -> NewAccount -> m (WalletResponse Account)
newAccount wId nAccount@NewAccount{..} = do
    let spendingPw = fromMaybe mempty naccSpendingPassword
    accInit <- migrate (wId, nAccount)
    cAccount <- V0.newAccount V0.RandomSeed spendingPw accInit
    single <$> (migrate cAccount)

updateAccount
    :: (V0.MonadWalletLogic ctx m)
    => WalletId -> AccountId -> AccountUpdate -> m (WalletResponse Account)
updateAccount wId accId accUpdate = do
    newAccId <- migrate (wId, accId)
    accMeta <- migrate accUpdate
    cAccount <- V0.updateAccount newAccId accMeta
    single <$> (migrate cAccount)
