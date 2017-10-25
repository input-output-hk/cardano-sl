{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
module Cardano.Wallet.API.V1.Handlers where

import           Universum

import           Cardano.Wallet.API.Types
import qualified Cardano.Wallet.API.V1                    as V1
import qualified Cardano.Wallet.API.V1.Handlers.Addresses as Addresses
import qualified Cardano.Wallet.API.V1.Handlers.Payments  as Payments
import qualified Cardano.Wallet.API.V1.Handlers.Wallets   as Wallets

import           Servant

handlers :: Server V1.API
handlers = apiVersion
      :<|> Addresses.handlers
      :<|> Wallets.handlers
      :<|> Payments.handlers

apiVersion :: Handler WalletVersion
apiVersion = return (WalletVersion V1 "6f1131adca2f0bc6d24c9181cabd2b9e0704fd79")
