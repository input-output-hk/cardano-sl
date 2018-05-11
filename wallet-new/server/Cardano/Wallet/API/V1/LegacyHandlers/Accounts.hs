module Cardano.Wallet.API.V1.LegacyHandlers.Accounts (
      handlers
    ) where

import           Universum

import           Cardano.Wallet.API.Request
import           Cardano.Wallet.API.Response
import qualified Cardano.Wallet.API.V1.Accounts as Accounts
import           Cardano.Wallet.API.V1.Errors
import           Cardano.Wallet.API.V1.Migration
import           Cardano.Wallet.API.V1.Types
import qualified Data.IxSet.Typed as IxSet

import qualified Pos.Wallet.Web.Account as V0
import qualified Pos.Wallet.Web.ClientTypes.Types as V0
import qualified Pos.Wallet.Web.Methods.Logic as V0
import           Servant
import           Test.QuickCheck (arbitrary, generate)

handlers
    :: (HasCompileInfo, HasConfigurations)
    => ServerT Accounts.API MonadV1
handlers =
         deleteAccount
    :<|> getAccount
    :<|> listAccounts
    :<|> newAccount
    :<|> updateAccount
    :<|> newExternalAccount
    :<|> newAddressPath

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
listAccounts wId params =
    let accounts = migrate wId >>= V0.getAccounts . Just >>= migrate @[V0.CAccount] @[Account]
    in respondWith params (NoFilters :: FilterOperations Account)
                          (NoSorts :: SortOperations Account)
                          (IxSet.fromList <$> accounts)

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


newExternalAccount
    :: (V0.MonadWalletLogic ctx m)
    => WalletId
    -> NewAccount
    -> m (WalletResponse Account)
newExternalAccount _ _ = do
    single <$> (liftIO $ generate arbitrary)


-- | Creates a new BIP44 derivation path for an external wallet.
--
-- Since this is a user endpoint, we do not allow to create internal / change
-- addresses. Therefore, the change path is always `0`.
newAddressPath
    :: (MonadThrow m, V0.MonadWalletLogic ctx m)
    => WalletId
    -> AccountIndex
    -> m (WalletResponse AddressPath)
newAddressPath wId accIdx = do
    account <- wrData <$> getAccount wId accIdx
    case mkAddressPathBIP44 (IsChangeAddress False) account of
        Left msg ->
            throwM $ CannotCreateAddress msg
        Right path ->
            return $ single path
