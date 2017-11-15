{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
module Cardano.Wallet.API.V1.Handlers where

import           Universum


import           Cardano.Wallet.API.Types
import qualified Cardano.Wallet.API.V1                    as V1
import qualified Cardano.Wallet.API.V1.Handlers.Addresses as Addresses
import qualified Cardano.Wallet.API.V1.Handlers.Payments  as Payments
import qualified Cardano.Wallet.API.V1.Handlers.Updates   as Updates
import qualified Cardano.Wallet.API.V1.Handlers.Wallets   as Wallets
import qualified Cardano.Wallet.API.V1.Wallets            as Wallets

import           Cardano.Wallet.API.V1.Migration

import           Servant

-- | Until we depend from V0 logic to implement the each 'Handler' we
-- still need the natural transformation here.
handlers :: ( HasConfigurations
            , HasCompileInfo
            )
            => (forall a. MonadV1 a -> Handler a)
            -> Server V1.API
handlers naturalTransformation = apiVersion
                            :<|> Addresses.handlers
                            :<|> hoistServer (Proxy @Wallets.API) naturalTransformation Wallets.handlers
                            :<|> Payments.handlers
                            :<|> Updates.handlers

apiVersion :: Handler WalletVersion
apiVersion = return (WalletVersion V1 "6f1131adca2f0bc6d24c9181cabd2b9e0704fd79")
