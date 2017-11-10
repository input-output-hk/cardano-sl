module Cardano.Wallet.API.V1.Handlers.Wallets where

import           Universum

import qualified Pos.Wallet.Web.ClientTypes.Types        as V0
import qualified Pos.Wallet.Web.Methods.Restore          as V0
import           Pos.Wallet.Web.Mode                     (WalletWebMode)

import qualified Cardano.Wallet.API.V1.Handlers.Accounts as Accounts
import           Cardano.Wallet.API.V1.Types             as V1
import qualified Cardano.Wallet.API.V1.Wallets           as Wallets

import           Servant
import           Test.QuickCheck                         (arbitrary, generate, resize)

-- | All the @Servant@ handlers for wallet-specific operations.
handlers :: ServerT Wallets.API WalletWebMode
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
newWallet :: NewWallet -> WalletWebMode Wallet
newWallet NewWallet{..} = liftIO $ generate arbitrary
-- newWallet :: L.MonadWalletLogic ctx m => PassPhrase -> CWalletInit -> m CWallet
  let spendingPassword = V0.CPassPhrase <$> newwalSpendingPassword
      initMeta   = V0.CWalletMeta newwalName newwalAssuranceLevel 0
  let walletInt  = V0.CWalletInit initMeta newwalBackupPhrase
  migrate <$> V0.newWallet spendingPassword walletInit
  where
    migrate :: V0.CWallet -> V1.Wallet
    migrate _ = _

listWallets :: PaginationParams
            -> WalletWebMode (OneOf [Wallet] (ExtendedResponse [Wallet]))
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
               -> WalletWebMode Wallet
updatePassword _ _ = liftIO $ generate arbitrary

deleteWallet :: WalletId
             -> WalletWebMode NoContent
deleteWallet _ = return NoContent

getWallet :: WalletId
          -> WalletWebMode Wallet
getWallet _ = liftIO $ generate arbitrary

updateWallet :: WalletId
             -> WalletUpdate
             -> WalletWebMode Wallet
updateWallet _ _ = liftIO $ generate arbitrary
