module Cardano.Wallet.API.V1.LegacyHandlers.Accounts
    ( handlers
    , newAccount
    ) where

import           Universum

import qualified Data.IxSet.Typed as IxSet
import           Servant

import           Pos.Chain.Txp (TxpConfiguration)
import           Pos.Core (decodeTextAddress)
import           Pos.Core.Txp (TxAux)
import           Pos.Crypto (ProtocolMagic)
import qualified Pos.Util.Servant as V0
import qualified Pos.Wallet.Web.Account as V0
import qualified Pos.Wallet.Web.ClientTypes.Types as V0
import qualified Pos.Wallet.Web.Methods.Logic as V0
import qualified Pos.Wallet.Web.Methods.Redeem as V0

import           Cardano.Wallet.API.Request
import           Cardano.Wallet.API.Response
import qualified Cardano.Wallet.API.V1.Accounts as Accounts
import           Cardano.Wallet.API.V1.Errors
import           Cardano.Wallet.API.V1.Migration
import           Cardano.Wallet.API.V1.Types

handlers
    :: HasConfigurations
    => ProtocolMagic
    -> TxpConfiguration
    -> (TxAux -> MonadV1 Bool)
    -> ServerT Accounts.API MonadV1
handlers pm txpConfig submitTx =
         deleteAccount
    :<|> getAccount
    :<|> listAccounts
    :<|> newAccount
    :<|> updateAccount
    :<|> redeemAda pm txpConfig submitTx
    :<|> newAddressPath
    :<|> storeNewAddress

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

redeemAda
    :: HasConfigurations
    => ProtocolMagic
    -> TxpConfiguration
    -> (TxAux -> MonadV1 Bool)
    -> WalletId
    -> AccountIndex
    -> Redemption
    -> MonadV1 (WalletResponse Transaction)
redeemAda pm txpConfig submitTx walletId accountIndex r = do
    let ShieldedRedemptionCode seed = redemptionRedemptionCode r
        V1 spendingPassword = redemptionSpendingPassword r
    accountId <- migrate (walletId, accountIndex)
    let caccountId = V0.encodeCType accountId
    fmap single . migrate =<< case redemptionMnemonic r of
        Just (RedemptionMnemonic mnemonic) -> do
            let phrase = V0.CBackupPhrase mnemonic
            let cpaperRedeem = V0.CPaperVendWalletRedeem
                    { V0.pvWalletId = caccountId
                    , V0.pvSeed = seed
                    , V0.pvBackupPhrase = phrase
                    }
            V0.redeemAdaPaperVend pm txpConfig submitTx spendingPassword cpaperRedeem
        Nothing -> do
            let cwalletRedeem = V0.CWalletRedeem
                    { V0.crWalletId = caccountId
                    , V0.crSeed = seed
                    }
            V0.redeemAda pm txpConfig submitTx spendingPassword cwalletRedeem

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
    acc <- wrData <$> getAccount wId accIdx
    case mkAddressPathBIP44 (IsChangeAddress False) acc of
        Left msg   -> throwM $ CannotCreateAddress msg
        Right path -> return $ single path

-- | After external wallet generated new address (using secret key and derivation path)
-- we have to store this new address. It will be returned in the @/api/v1/addresses@ result.
storeNewAddress
    :: (MonadThrow m, V0.MonadWalletLogic ctx m)
    => WalletId
    -> AccountIndex
    -> Text
    -> m NoContent
storeNewAddress wId accIdx newAddressAsText = do
    accId <- migrate (wId, accIdx)
    newAddress <- either (throwM . InvalidAddressFormat)
                         pure
                         (decodeTextAddress newAddressAsText)
    V0.storeNewAddress accId newAddress
