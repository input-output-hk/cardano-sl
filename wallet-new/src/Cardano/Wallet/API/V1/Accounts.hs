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
    =    "accounts" :> Capture "accountId" Text
                    :> Summary "Deletes an Account."
                    :> DeleteNoContent '[JSON] NoContent
    :<|> "accounts" :> Capture "accountId" Text
                    :> Summary "Retrieves a specific Account, given its Id."
                    :> Get '[JSON] Account
    :<|> "accounts" :> WalletRequestParams
                    :> Summary "Retrieves the full list of Accounts."
                    :> Get '[JSON] (OneOf [Account] (ExtendedResponse [Account]))
