{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Cardano.Wallet.API.V1.Handlers.Accounts where

import qualified Cardano.Wallet.API.V1.Accounts as Accounts
import Cardano.Wallet.API.V1.Types
import Cardano.Wallet.API.Types

import Servant
import Data.Text

handlers :: Server Accounts.API
handlers =   deleteAccount
       :<|>  listAccounts

deleteAccount :: Text -> Handler NoContent
deleteAccount _ = return NoContent

listAccounts :: Maybe Int
             -> Maybe Int
             -> Maybe Bool
             -> Maybe Text
             -> Handler (OneOf [Account] (ExtendedResponse [Account]))
listAccounts _ _ mbExtended _ =
  case mbExtended of
    Just True  -> return $ OneOf $ Right $
      ExtendedResponse {
        ext_data = [Account "deadBeef", Account "123AABBCC"]
      , ext_meta = Metadata {
          meta_total_pages = 1
        , meta_page = 1
        , meta_per_page = 20
        , meta_total_entries = 2
      }
      }
    _ -> return $ OneOf $ Left  [Account "deadBeef", Account "123AABBCC"]
