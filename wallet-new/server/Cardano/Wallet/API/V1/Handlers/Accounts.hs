module Cardano.Wallet.API.V1.Handlers.Accounts (
      handlers
    ) where

import           Universum

import qualified Cardano.Wallet.API.V1.Accounts as Accounts
import           Cardano.Wallet.API.V1.Errors as Errors
import           Cardano.Wallet.API.V1.Types


import qualified Pos.Core as Core
import           Servant
import           Test.QuickCheck (arbitrary, generate, listOf1, resize)

handlers :: WalletId -> Server Accounts.API
handlers walletId =
          deleteAccount walletId
    :<|>  getAccount walletId
    :<|>  listAccounts
    :<|>  newAccount walletId
    :<|>  updateAccount walletId

deleteAccount :: WalletId -> AccountId -> Handler NoContent
deleteAccount _ _ = return NoContent

getAccount :: WalletId -> AccountId -> Handler Account
getAccount _ _ = liftIO $ generate arbitrary

listAccounts :: PaginationParams
             -> Handler (OneOf [Account] (ExtendedResponse [Account]))
listAccounts PaginationParams {..} = do
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

-- | This is an example of how POST requests might look like.
-- It also shows an example of how an error might look like.
-- NOTE: This will probably change drastically as soon as we start using our
-- custom monad as a base of the Handler stack, so the example here is just to
-- give the idea of how it will look like on Swagger.
newAccount :: WalletId -> Maybe Text -> AccountUpdate -> Handler Account
newAccount w@(WalletId wId) _ AccountUpdate{..} = do
    when (wId /= "testwallet") $ throwError $ Errors.toError Errors.WalletNotFound
    -- In real code we would generate things like addresses (if needed) or
    -- any other form of Id/data.
    newId <- liftIO $ generate (listOf1 arbitrary)
    return $ Account {
             accId = fromString newId
           , accAmount = Core.mkCoin 0
           , accAddresses = mempty
           , accName = uaccName
           , accWalletId = w
           }

updateAccount :: WalletId -> AccountId -> AccountUpdate -> Handler Account
updateAccount w _ u = newAccount w Nothing u
