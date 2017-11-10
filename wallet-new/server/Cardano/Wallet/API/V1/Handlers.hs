{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
module Cardano.Wallet.API.V1.Handlers where

import           Universum

import           Pos.Wallet.Web.Mode                      (WalletWebMode)

import           Cardano.Wallet.API.Types
import qualified Cardano.Wallet.API.V1                    as V1
import qualified Cardano.Wallet.API.V1.Handlers.Addresses as Addresses
import qualified Cardano.Wallet.API.V1.Handlers.Payments  as Payments
import qualified Cardano.Wallet.API.V1.Handlers.Updates   as Updates
import qualified Cardano.Wallet.API.V1.Handlers.Wallets   as Wallets

import           Servant

-- | Until we depend from V0 logic to implement the each 'Handler' we
-- still need the natural transformation here.
handlers :: (WalletWebMode :~> Handler)
         -> Server V1.API
handlers naturalTransformation = apiVersion
                            :<|> Addresses.handlers
                            :<|> enter naturalTransformation Wallets.handlers
                            :<|> Payments.handlers
                            :<|> Updates.handlers

apiVersion :: Handler WalletVersion
apiVersion = return (WalletVersion V1 "6f1131adca2f0bc6d24c9181cabd2b9e0704fd79")
