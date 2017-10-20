module Cardano.Wallet.API.V1.Handlers.Accounts where

import           Universum

import qualified Cardano.Wallet.API.V1.Accounts as Accounts
import           Cardano.Wallet.API.V1.Types

import           Servant
import           Test.QuickCheck                (arbitrary, generate, resize)

handlers :: Server Accounts.API
handlers =   deleteAccount
       :<|>  getAccount
       :<|>  listAccounts

deleteAccount :: Text -> Handler NoContent
deleteAccount _ = return NoContent

getAccount :: AccountId
           -> Handler Account
getAccount _ = liftIO $ generate arbitrary

listAccounts :: Maybe Page
             -> Maybe PerPage
             -> Maybe Bool
             -> Maybe Text
             -> Handler (OneOf [Account] (ExtendedResponse [Account]))
listAccounts _ _ mbExtended _ = do
  example <- liftIO $ generate (resize 3 arbitrary)
  case mbExtended of
    Just True  -> return $ OneOf $ Right $
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
