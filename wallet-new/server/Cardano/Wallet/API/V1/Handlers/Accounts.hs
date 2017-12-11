module Cardano.Wallet.API.V1.Handlers.Accounts (
      handlers
    ) where

import           Universum

import           Cardano.Wallet.API.Request
import           Cardano.Wallet.API.Response
import qualified Cardano.Wallet.API.V1.Accounts as Accounts
import           Cardano.Wallet.API.V1.Types
import           Cardano.Wallet.API.V1.Migration

import qualified Pos.Wallet.Web.Methods.Logic as V0
import qualified Pos.Wallet.Web.Account as V0
import           Servant
import           Test.QuickCheck (arbitrary, generate, resize)

handlers
    :: (HasCompileInfo, HasConfigurations)
    => WalletId -> ServerT Accounts.API MonadV1
handlers walletId =
          deleteAccount walletId
    :<|>  getAccount walletId
    :<|>  listAccounts
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

listAccounts :: RequestParams
             -> MonadV1 (WalletResponse [Account])
listAccounts RequestParams {..} = do
  example <- liftIO $ generate (resize 3 arbitrary)
  return WalletResponse {
        wrData = example
      , wrStatus = SuccessStatus
      , wrMeta = Metadata $ PaginationMetadata {
          metaTotalPages = 1
        , metaPage = 1
        , metaPerPage = 20
        , metaTotalEntries = 3
        }
      }

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
