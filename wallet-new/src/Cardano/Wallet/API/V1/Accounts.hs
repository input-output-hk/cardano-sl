{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Cardano.Wallet.API.V1.Accounts where

import           Cardano.Wallet.API.Types
import           Cardano.Wallet.API.V1.Types

import           Data.Text
import           Servant

type API
    =    "accounts" :> Capture "account_id" Text
                    :> Summary "Deletes an Account."
                    :> DeleteNoContent '[JSON] NoContent
    -- TODO: Is there a way to scrap up this boilerplate?
    :<|> "accounts" :> QueryParam "page"     Int
                    :> QueryParam "per_page" Int
                    :> QueryParam "extended" Bool
                    :> Header "Daedalus-Response-Format" Text
                    :> Summary "Retrieves the full list of Accounts."
                    :> Get '[JSON] (OneOf [Account] (ExtendedResponse [Account]))
    -- :<|> "accounts" :> ReqBody '[JSON] Account :> Post '[JSON] Account
    -- :<|> "accounts" :> Capture "account_id" Text :> Verb 'GET 200 '[JSON] Account
    -- :<|> "accounts" :> Capture "account_id" Text :> Verb 'PUT 200 '[JSON] Account
