module Cardano.Wallet.API.V1.Handlers.Accounts (
      handlers
    ) where

import           Universum

import           Cardano.Wallet.API.Request
import           Cardano.Wallet.API.Response
import qualified Cardano.Wallet.API.V1.Accounts as Accounts
import           Cardano.Wallet.API.V1.Errors as Errors
import           Cardano.Wallet.API.V1.Types
import           Cardano.Wallet.API.V1.Migration

import qualified Pos.Wallet.Web.Methods.Logic as V0
import qualified Pos.Wallet.Web.Tracking as V0
import qualified Pos.Wallet.Web.Account as V0
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
    migrate (wId, accId) >>= V0.fixingCachedAccModifier V0.getAccount >>= migrate

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

newAccount
    :: (MonadThrow m, V0.MonadWalletLogic ctx m)
    => WalletId -> Maybe PassPhrase -> AccountUpdate -> m Account
newAccount wId mPassPhrase accUpdate =
    case mPassPhrase of
        Nothing -> throwM $ Errors.toError Errors.NoPassPhrase
        Just passPhrase -> do
            newPassPhrase <- migrate passPhrase
            accInit <- migrate (wId, accUpdate)
            cAccount <- V0.newAccount V0.RandomSeed newPassPhrase accInit
            migrate cAccount

updateAccount
   :: (MonadThrow m, V0.MonadWalletLogic ctx m)
    => WalletId -> AccountId -> AccountUpdate -> m Account
updateAccount w _ = newAccount w Nothing
