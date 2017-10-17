{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Cardano.Wallet.API.V1 where

import           Cardano.Wallet.API.Types
import qualified Cardano.Wallet.API.V1.Accounts  as Accounts
import qualified Cardano.Wallet.API.V1.Addresses as Addresses

import           Servant

type API = "version"
           :> Summary "Returns the version for this API."
           :> Get '[JSON] APIVersion
       :<|> Accounts.API
       :<|> Addresses.API
