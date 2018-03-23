module Cardano.Wallet.API.V1.LegacyHandlers.Accounts
    ( handlers
    , newAccount
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
import           Servant

handlers
    :: (HasCompileInfo, HasConfigurations)
    => ServerT Accounts.API MonadV1
handlers =
         deleteAccount
    :<|> getAccount
    :<|> listAccounts
    :<|> newAccount
    :<|> updateAccount

deleteAccount
    :: (V0.MonadWalletLogic ctx m)
    => WalletId -> AccountIndex -> m NoContent
deleteAccount wId accIdx =
    migrate (wId, accIdx) >>= V0.deleteAccount

getAccount
    :: (MonadThrow m, V0.MonadWalletLogicRead ctx m)
    => WalletId -> AccountIndex -> m (WalletResponse Account)
getAccount wId accIdx =
    single <$> (migrate (wId, accIdx) >>= V0.getAccount >>= migrate)

listAccounts
    :: (MonadThrow m, V0.MonadWalletLogicRead ctx m)
    => WalletId -> RequestParams -> m (WalletResponse [Account])
listAccounts wId params = do
    wid' <- migrate wId
    oldAccounts <- V0.getAccounts (Just wid')
    newAccounts <- migrate @[V0.CAccount] @[Account] oldAccounts
    respondWith params
        (NoFilters :: FilterOperations Account)
        (NoSorts :: SortOperations Account)
        (IxSet.fromList <$> pure newAccounts)

newAccount
    :: (V0.MonadWalletLogic ctx m)
    => WalletId -> NewAccount -> m (WalletResponse Account)
newAccount wId nAccount@NewAccount{..} = do
    let (V1 spendingPw) = fromMaybe (V1 mempty) naccSpendingPassword
    accInit <- migrate (wId, nAccount)
    cAccount <- V0.newAccount V0.RandomSeed spendingPw accInit
    single <$> (migrate cAccount)

updateAccount
    :: (V0.MonadWalletLogic ctx m)
    => WalletId -> AccountIndex -> AccountUpdate -> m (WalletResponse Account)
updateAccount wId accIdx accUpdate = do
    newAccId <- migrate (wId, accIdx)
    accMeta <- migrate accUpdate
    cAccount <- V0.updateAccount newAccId accMeta
    single <$> (migrate cAccount)
