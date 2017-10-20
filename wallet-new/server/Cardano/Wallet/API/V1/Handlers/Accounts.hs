module Cardano.Wallet.API.V1.Handlers.Accounts where

import           Universum

import qualified Cardano.Wallet.API.V1.Accounts as Accounts
import           Cardano.Wallet.API.V1.Types

import           Servant
import           Test.QuickCheck                (arbitrary, generate, resize)

handlers :: Server Accounts.API
handlers =   deleteAccount
       :<|>  listAccounts

deleteAccount :: Text -> Handler NoContent
deleteAccount _ = return NoContent

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
        ext_data = example
      , ext_meta = Metadata {
          meta_total_pages = 1
        , meta_page = 1
        , meta_per_page = 20
        , meta_total_entries = 3
      }
      }
    _ -> return $ OneOf $ Left example
