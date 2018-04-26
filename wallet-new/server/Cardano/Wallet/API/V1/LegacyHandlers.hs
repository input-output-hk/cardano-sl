{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}

module Cardano.Wallet.API.V1.LegacyHandlers where

import           Universum

import           Ntp.Client (NtpStatus)
import           Pos.Diffusion.Types (Diffusion (sendTx))

import qualified Cardano.Wallet.API.V1 as V1
import qualified Cardano.Wallet.API.V1.Accounts as Accounts
import qualified Cardano.Wallet.API.V1.Addresses as Addresses
import qualified Cardano.Wallet.API.V1.Info as Info
import qualified Cardano.Wallet.API.V1.LegacyHandlers.Accounts as Accounts
import qualified Cardano.Wallet.API.V1.LegacyHandlers.Addresses as Addresses
import qualified Cardano.Wallet.API.V1.LegacyHandlers.Info as Info
import qualified Cardano.Wallet.API.V1.LegacyHandlers.Settings as Settings
import qualified Cardano.Wallet.API.V1.LegacyHandlers.Transactions as Transactions
import qualified Cardano.Wallet.API.V1.LegacyHandlers.Wallets as Wallets
import qualified Cardano.Wallet.API.V1.Settings as Settings
import qualified Cardano.Wallet.API.V1.Transactions as Transactions
import qualified Cardano.Wallet.API.V1.Wallets as Wallets

-- importing the orphan instance for ToHttpApiData Core.Coin
import           Cardano.Wallet.API.V1.Types ()

import           Cardano.Wallet.API.V1.Migration

import           Servant

-- | Until we depend from V0 logic to implement the each 'Handler' we
-- still need the natural transformation here.
handlers :: ( HasConfigurations
            , HasCompileInfo
            )
            => (forall a. MonadV1 a -> Handler a)
            -> Diffusion MonadV1
            -> TVar NtpStatus
            -> Server V1.API
handlers naturalTransformation diffusion ntpStatus =
         hoistServer (Proxy @Addresses.API) naturalTransformation Addresses.handlers
    :<|> hoistServer (Proxy @Wallets.API) naturalTransformation Wallets.handlers
    :<|> hoistServer (Proxy @Accounts.API) naturalTransformation Accounts.handlers
    :<|> hoistServer (Proxy @Transactions.API) naturalTransformation (Transactions.handlers (sendTx diffusion))
    :<|> hoistServer (Proxy @Settings.API) naturalTransformation Settings.handlers
    :<|> hoistServer (Proxy @Info.API) naturalTransformation (Info.handlers diffusion ntpStatus)
