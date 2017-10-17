{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Cardano.Wallet.API.V1.Handlers where

import Cardano.Wallet.API (WalletAPI)
import Cardano.Wallet.API.Types (APIVersion(..))
import qualified Cardano.Wallet.API.V1 as V1
import qualified Cardano.Wallet.API.V1.Handlers.Accounts as Accounts
import qualified Cardano.Wallet.API.V1.Handlers.Addresses as Addresses

import Servant

handlers :: Server V1.API
handlers = apiVersion
      :<|> Accounts.handlers
      :<|> Addresses.handlers

apiVersion :: Handler APIVersion
apiVersion = return V1
