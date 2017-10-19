{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Cardano.Wallet.API.V1.Accounts where

import           Cardano.Wallet.API.Types
import           Cardano.Wallet.API.V1.Parameters
import           Cardano.Wallet.API.V1.Types

import           Data.Text
import           Servant

type API
    =    "accounts" :> Capture "account_id" Text
                    :> Summary "Deletes an Account."
                    :> DeleteNoContent '[JSON] NoContent
    -- TODO: Is there a way to scrap up this boilerplate?
    :<|> "accounts" :> WalletRequestParams
                    :> Summary "Retrieves the full list of Accounts."
                    :> Get '[JSON] (OneOf [Account] (ExtendedResponse [Account]))
    -- :<|> "accounts" :> ReqBody '[JSON] Account :> Post '[JSON] Account
    -- :<|> "accounts" :> Capture "account_id" Text :> Verb 'GET 200 '[JSON] Account
    -- :<|> "accounts" :> Capture "account_id" Text :> Verb 'PUT 200 '[JSON] Account
