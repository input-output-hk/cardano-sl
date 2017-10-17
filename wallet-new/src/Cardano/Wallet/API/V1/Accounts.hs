{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Cardano.Wallet.API.V1.Accounts where

import Cardano.Wallet.API.V1.Types

import Servant
import Data.Text

type API
    =    "accounts" :> Capture "account_id" Text
                    :> DeleteNoContent '[JSON] NoContent
    -- TODO: Is there a way to scrap up this boilerplate?
    :<|> "accounts" :> QueryParam "page"     Int
                    :> QueryParam "per_page" Int
                    :> QueryParam "extended" Bool
                    :> Header "Daedalus-Response-Format" Text
                    :> Get '[JSON] (OneOf [Account] (ExtendedResponse [Account]))
    -- :<|> "accounts" :> ReqBody '[JSON] Account :> Post '[JSON] Account
    -- :<|> "accounts" :> Capture "account_id" Text :> Verb 'GET 200 '[JSON] Account
    -- :<|> "accounts" :> Capture "account_id" Text :> Verb 'PUT 200 '[JSON] Account
