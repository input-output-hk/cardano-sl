{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}

module Cardano.Wallet.API.V1.LegacyHandlers where

import           Universum

import           Ntp.Client (NtpStatus)
import           Pos.Core.NetworkMagic (makeNetworkMagic)
import           Pos.Crypto (ProtocolMagic)
import           Pos.Infra.Diffusion.Types (Diffusion (sendTx))

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
            -> ProtocolMagic
            -> Diffusion MonadV1
            -> TVar NtpStatus
            -> Server V1.API
handlers naturalTransformation pm diffusion ntpStatus =
         hoistServer (Proxy @Addresses.API) naturalTransformation (Addresses.handlers nm)
    :<|> hoistServer (Proxy @Wallets.API) naturalTransformation (Wallets.handlers nm)
    :<|> hoistServer (Proxy @Accounts.API) naturalTransformation (Accounts.handlers nm)
    :<|> hoistServer (Proxy @Transactions.API) naturalTransformation (Transactions.handlers pm (sendTx diffusion))
    :<|> hoistServer (Proxy @Settings.API) naturalTransformation Settings.handlers
    :<|> hoistServer (Proxy @Info.API) naturalTransformation (Info.handlers diffusion ntpStatus)
  where
    nm = makeNetworkMagic pm
