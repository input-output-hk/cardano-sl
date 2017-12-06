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

import qualified Pos.Core as Core
import           Pos.Wallet.Web.Methods.Logic (MonadWalletLogic)
import           Servant
import           Test.QuickCheck (arbitrary, generate, vectorOf)
import           Test.QuickCheck.Gen (unGen)
import           Test.QuickCheck.Random (mkQCGen)

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
newWallet :: (MonadThrow m, MonadWalletLogic ctx m) => NewWallet -> m Wallet
newWallet NewWallet{..} = do
  let spendingPassword = fromMaybe mempty newwalSpendingPassword
  initMeta <- V0.CWalletMeta <$> pure newwalName
                             <*> migrate newwalAssuranceLevel
                             <*> pure 0
  let walletInit = V0.CWalletInit initMeta newwalBackupPhrase
  V0.newWallet spendingPassword walletInit >>= migrate

-- TODO(adinapoli): Implement this properly with CSL-1891.
-- Providing here just a stub.
listWallets :: RequestParams
            -> MonadV1 (WalletResponse [Wallet])
listWallets params = do
    -- Use a static seed to simulate the pagination properly.
    -- Use `pure` to simulate a monadic action.
    let zipped  = zip [1..] (unGen (vectorOf 100000 arbitrary) (mkQCGen 42) 42)
    let dataSet = pure $ map (\(idx, w) -> w { walBalance = Core.mkCoin idx}) zipped
    respondWith params (const dataSet)

updatePassword
    :: (MonadWalletLogic ctx m)
    => WalletId -> PasswordUpdate -> m Wallet
updatePassword wid PasswordUpdate{..} = do
    wid' <- migrate wid
    _ <- V0.changeWalletPassphrase wid' pwdOld pwdNew
    V0.getWallet wid' >>= migrate

-- | Deletes an exisiting wallet.
deleteWallet
    :: (MonadWalletLogic ctx m)
    => WalletId
    -> m NoContent
deleteWallet (WalletId walletId) =
    V0.deleteWallet . V0.CId . V0.CHash $ walletId

getWallet
    :: WalletId
    -> MonadV1 Wallet
getWallet _ = liftIO $ generate arbitrary

updateWallet
    :: WalletId
    -> WalletUpdate
    -> MonadV1 Wallet
updateWallet _ _ = liftIO $ generate arbitrary
