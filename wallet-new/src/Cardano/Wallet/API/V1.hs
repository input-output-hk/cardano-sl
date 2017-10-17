{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Cardano.Wallet.API.V1 where

import Cardano.Wallet.API.Types (APIVersion)
import qualified Cardano.Wallet.API.V1.Accounts as Accounts
import qualified Cardano.Wallet.API.V1.Addresses as Addresses

import Servant

type API = "version" :> Get '[JSON] APIVersion
       :<|> Accounts.API
       :<|> Addresses.API
