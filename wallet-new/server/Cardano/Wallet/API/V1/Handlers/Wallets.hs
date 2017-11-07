module Cardano.Wallet.API.V1.Handlers.Wallets where

import           Universum

import qualified Cardano.Wallet.API.V1.Handlers.Accounts as Accounts
import           Cardano.Wallet.API.V1.Types
import qualified Cardano.Wallet.API.V1.Wallets           as Wallets

import           Servant
import           Test.QuickCheck                         (arbitrary, generate, resize)

handlers :: Server Wallets.API
handlers =   newWallet
        :<|> listWallets
        :<|> (\walletId -> do
                     updatePassword walletId
                :<|> deleteWallet walletId
                :<|> getWallet walletId
                :<|> updateWallet walletId
                :<|> Accounts.handlers walletId
             )

newWallet :: NewWallet -> Handler Wallet
newWallet _ = liftIO $ generate arbitrary


listWallets :: PaginationParams
            -> Handler (OneOf [Wallet] (ExtendedResponse [Wallet]))
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

updatePassword :: WalletId -> PasswordUpdate -> Handler Wallet
updatePassword _ _ = liftIO $ generate arbitrary

deleteWallet :: WalletId -> Handler NoContent
deleteWallet _ = return NoContent

getWallet :: WalletId -> Handler Wallet
getWallet _ = liftIO $ generate arbitrary

updateWallet :: WalletId -> WalletUpdate -> Handler Wallet
updateWallet _ _ = liftIO $ generate arbitrary
