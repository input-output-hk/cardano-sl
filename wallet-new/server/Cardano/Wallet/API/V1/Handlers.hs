{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Cardano.Wallet.API.V1.Handlers where

import           Cardano.Wallet.API.Types
import qualified Cardano.Wallet.API.V1                    as V1
import qualified Cardano.Wallet.API.V1.Handlers.Accounts  as Accounts
import qualified Cardano.Wallet.API.V1.Handlers.Addresses as Addresses

import           Servant

handlers :: Server V1.API
handlers = apiVersion
      :<|> Accounts.handlers
      :<|> Addresses.handlers

apiVersion :: Handler WalletVersion
apiVersion = return (WalletVersion V1 "6f1131adca2f0bc6d24c9181cabd2b9e0704fd79")
