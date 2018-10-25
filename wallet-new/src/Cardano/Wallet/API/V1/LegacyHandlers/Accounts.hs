module Cardano.Wallet.API.V1.LegacyHandlers.Accounts
    ( handlers
    , newAccount
    ) where

import           Universum

import           Servant

import           Pos.Core.NetworkMagic (NetworkMagic)
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

handlers :: NetworkMagic -> ServerT Accounts.API MonadV1
handlers nm =
         deleteAccount
    :<|> getAccount nm
    :<|> listAccounts nm
    :<|> newAccount nm
    :<|> updateAccount nm
    :<|> getAccountAddresses nm
    :<|> getAccountBalance nm

deleteAccount
    :: (V0.MonadWalletLogic ctx m)
    => WalletId
    -> AccountIndex
    -> m NoContent
deleteAccount wId accIdx =
    migrate (wId, accIdx) >>= V0.deleteAccount

getAccount
    :: (MonadThrow m, V0.MonadWalletLogicRead ctx m)
    => NetworkMagic
    -> WalletId
    -> AccountIndex
    -> m (WalletResponse Account)
getAccount nm wId accIdx =
    single <$> (migrate (wId, accIdx) >>= V0.getAccount nm >>= migrate)

listAccounts
    :: (MonadThrow m, V0.MonadWalletLogicRead ctx m)
    => NetworkMagic
    -> WalletId
    -> RequestParams
    -> m (WalletResponse [Account])
listAccounts nm wId params = do
    wid' <- migrate wId
    oldAccounts <- V0.getAccounts nm (Just wid')
    newAccounts <- migrate @[V0.CAccount] @[Account] oldAccounts
    respondWith params
        (NoFilters :: FilterOperations '[] Account)
        (NoSorts :: SortOperations Account)
        (IxSet.fromList <$> pure newAccounts)

newAccount
    :: (V0.MonadWalletLogic ctx m)
    => NetworkMagic
    -> WalletId
    -> NewAccount
    -> m (WalletResponse Account)
newAccount nm wId nAccount@NewAccount{..} = do
    let (V1 spendingPw) = fromMaybe (V1 mempty) naccSpendingPassword
    accInit <- migrate (wId, nAccount)
    cAccount <- V0.newAccount nm V0.RandomSeed spendingPw accInit
    single <$> (migrate cAccount)

updateAccount
    :: (V0.MonadWalletLogic ctx m)
    => NetworkMagic
    -> WalletId
    -> AccountIndex
    -> AccountUpdate
    -> m (WalletResponse Account)
updateAccount nm wId accIdx accUpdate = do
    newAccId <- migrate (wId, accIdx)
    accMeta <- migrate accUpdate
    cAccount <- V0.updateAccount nm newAccId accMeta
    single <$> (migrate cAccount)

getAccountAddresses
    :: (V0.MonadWalletLogic ctx m)
    => NetworkMagic
    -> WalletId
    -> AccountIndex
    -> RequestParams
    -> FilterOperations '[V1 Address] WalletAddress
    -> m (WalletResponse AccountAddresses)
getAccountAddresses nm wId accIdx pagination filters = do
    resp <- respondWith pagination filters NoSorts (getAddresses <$> getAccount nm wId accIdx)
    return resp { wrData = AccountAddresses . wrData $ resp }
  where
    getAddresses =
        IxSet.fromList . accAddresses . wrData

getAccountBalance
    :: (V0.MonadWalletLogic ctx m)
    => NetworkMagic
    -> WalletId
    -> AccountIndex
    -> m (WalletResponse AccountBalance)
getAccountBalance nm wId accIdx = do
    resp <- getAccount nm wId accIdx
    return resp { wrData = AccountBalance . accAmount . wrData $ resp }
