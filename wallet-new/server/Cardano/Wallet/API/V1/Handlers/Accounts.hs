module Cardano.Wallet.API.V1.Handlers.Accounts (
      handlers
    ) where

import           Universum

import           Cardano.Wallet.API.Request
import           Cardano.Wallet.API.Response
import qualified Cardano.Wallet.API.V1.Accounts as Accounts
import           Cardano.Wallet.API.V1.Migration
import           Cardano.Wallet.API.V1.Types


import qualified Pos.Core as Core
import qualified Pos.Wallet.Web.Methods.Logic as V0
import           Servant
import           Test.QuickCheck (arbitrary, generate, resize)

handlers
    :: (HasCompileInfo, HasConfigurations)
    => WalletId -> ServerT Accounts.API MonadV1
handlers walletId =
          deleteAccount walletId
    :<|>  getAccount walletId
    :<|>  listAccounts
    :<|>  newAccount walletId
    :<|>  updateAccount walletId

deleteAccount :: WalletId -> AccountId -> MonadV1 NoContent
deleteAccount _ _ = return NoContent

getAccount
    :: (MonadThrow m, V0.MonadWalletLogicRead ctx m)
    => WalletId -> AccountId -> m Account
getAccount wId accId =
    migrate (wId, accId) >>= V0.getAccount >>= migrate

listAccounts :: RequestParams
             -> MonadV1 (OneOf [Account] (ExtendedResponse [Account]))
listAccounts RequestParams {..} = do
  example <- liftIO $ generate (resize 3 arbitrary)
  case rpResponseFormat of
    Extended -> return $ OneOf $ Right $
      ExtendedResponse {
        extData = example
      , extStatus = SuccessStatus
      , extMeta = Metadata $ PaginationMetadata {
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
newAccount :: WalletId -> Maybe Text -> AccountUpdate -> MonadV1 Account
newAccount wId _ AccountUpdate{..} = do
    -- In real code we would generate things like addresses (if needed) or
    -- any other form of Id/data.
    newId <- liftIO $ generate arbitrary
    return Account
        { accId = newId
        , accAmount = Core.mkCoin 0
        , accAddresses = mempty
        , accName = uaccName
        , accWalletId = wId
        }

updateAccount :: WalletId -> AccountId -> AccountUpdate -> MonadV1 Account
updateAccount w _ u = newAccount w Nothing u
