module Cardano.Wallet.API.V1.Handlers.Wallets where

import           Universum

import qualified Pos.Wallet.Web.ClientTypes.Types as V0
import qualified Pos.Wallet.Web.Methods.Restore as V0

import           Cardano.Wallet.API.V1.Migration
import           Cardano.Wallet.API.V1.Types as V1
import qualified Cardano.Wallet.API.V1.Wallets as Wallets

import           Pos.Wallet.Web.Methods.Logic (MonadWalletLogic)
import           Servant
import           Test.QuickCheck (arbitrary, generate, resize)

-- | All the @Servant@ handlers for wallet-specific operations.
handlers :: ( HasConfigurations
            , HasCompileInfo
            )
         => ServerT Wallets.API MonadV1
handlers =   newWallet
        :<|> listWallets
        :<|> (\walletId -> do
                     updatePassword walletId
                :<|> deleteWallet walletId
                :<|> getWallet walletId
                :<|> updateWallet walletId
                -- :<|> Accounts.handlers walletId
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

listWallets :: PaginationParams
            -> MonadV1 (OneOf [Wallet] (ExtendedResponse [Wallet]))
listWallets PaginationParams {..} = do
  example <- liftIO $ generate (resize 3 arbitrary)
  case ppResponseFormat of
    Extended -> return $ OneOf $ Right $
      ExtendedResponse {
        extData = example
      , extMeta = Metadata {
          metaTotalPages = 1
        , metaPage = 1
        , metaPerPage = 20
        , metaTotalEntries = 3
      }
      }
    _ -> return $ OneOf $ Left example

updatePassword :: WalletId
               -> PasswordUpdate
               -> MonadV1 Wallet
updatePassword _ _ = liftIO $ generate arbitrary

deleteWallet :: WalletId
             -> MonadV1 NoContent
deleteWallet _ = return NoContent

getWallet :: WalletId
          -> MonadV1 Wallet
getWallet _ = liftIO $ generate arbitrary

updateWallet :: WalletId
             -> WalletUpdate
             -> MonadV1 Wallet
updateWallet _ _ = liftIO $ generate arbitrary
