{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Cardano.Wallet.API.V1.Accounts where

import           Universum

import           Cardano.Wallet.API.Types
import           Cardano.Wallet.API.V1.Parameters
import           Cardano.Wallet.API.V1.Types

import           Servant

type API
    =    "accounts" :> Capture "accountId" AccountId
                    :> Summary "Deletes an Account."
                    :> DeleteNoContent '[JSON] NoContent
    :<|> "accounts" :> Capture "accountId" Text
                    :> Summary "Retrieves a specific Account, given its Id."
                    :> Get '[JSON] Account
    :<|> "accounts" :> WalletRequestParams
                    :> Summary "Retrieves the full list of Accounts."
                    :> Get '[JSON] (OneOf [Account] (ExtendedResponse [Account]))
    :<|> "accounts" :> Header  "Daedalus-Passphrase" Text
                    :> Summary "Creates a new Account for the given Wallet."
                    :> ReqBody '[JSON] (New Account)
                    :> Post '[JSON] Account
    :<|> "accounts" :> Capture "accountId" AccountId
                    :> Summary "Update an Account for the given Wallet."
                    :> ReqBody '[JSON] (Update Account)
                    :> Put '[JSON] Account
