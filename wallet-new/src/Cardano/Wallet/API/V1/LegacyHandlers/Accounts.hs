module Cardano.Wallet.API.V1.LegacyHandlers.Accounts
    ( handlers
    , newAccount
    ) where

import           Universum

import           Servant

import qualified Pos.Wallet.Web.Account as V0
import qualified Pos.Wallet.Web.ClientTypes.Types as V0
import qualified Pos.Wallet.Web.Methods.Logic as V0

import           Cardano.Wallet.API.Request
import           Cardano.Wallet.API.Response
import qualified Cardano.Wallet.API.V1.Accounts as Accounts
import           Cardano.Wallet.API.V1.LegacyHandlers.Instances ()
import           Cardano.Wallet.API.V1.Migration
import           Cardano.Wallet.API.V1.Types
import qualified Cardano.Wallet.Kernel.DB.Util.IxSet as IxSet

handlers :: ServerT Accounts.API MonadV1
handlers =
         deleteAccount
    :<|> getAccount
    :<|> listAccounts
    :<|> newAccount
    :<|> updateAccount
    :<|> getAccountAddresses
    :<|> getAccountBalance

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
        (NoFilters :: FilterOperations '[] Account)
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

getAccountAddresses
    :: (V0.MonadWalletLogic ctx m)
    => WalletId
    -> AccountIndex
    -> RequestParams
    -> FilterOperations '[V1 Address] WalletAddress
    -> m (WalletResponse AccountAddresses)
getAccountAddresses wId accIdx pagination filters = do
    resp <- respondWith pagination filters NoSorts (getAddresses <$> getAccount wId accIdx)
    return resp { wrData = AccountAddresses . wrData $ resp }
  where
    getAddresses =
        IxSet.fromList . accAddresses . wrData

getAccountBalance
    :: (V0.MonadWalletLogic ctx m)
    => WalletId
    -> AccountIndex
    -> m (WalletResponse AccountBalance)
getAccountBalance wId accIdx = do
    resp <- getAccount wId accIdx
    return resp { wrData = AccountBalance . accAmount . wrData $ resp }
