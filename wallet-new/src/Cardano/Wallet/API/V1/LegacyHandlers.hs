{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}

module Cardano.Wallet.API.V1.LegacyHandlers where

import           Universum

import           Ntp.Client (NtpStatus)
import           Pos.Chain.Txp (TxpConfiguration)
import           Pos.Crypto (ProtocolMagic)
import           Pos.Infra.Diffusion.Types (Diffusion (sendTx))
import           Pos.Util.Trace.Named (TraceNamed)

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
            => TraceNamed MonadV1
            -> (forall a. MonadV1 a -> Handler a)
            -> ProtocolMagic
            -> TxpConfiguration
            -> Diffusion MonadV1
            -> TVar NtpStatus
            -> Server V1.API
handlers logTrace naturalTransformation pm txpConfig diffusion ntpStatus =
         hoist' (Proxy @Addresses.API) (Addresses.handlers logTrace)
    :<|> hoist' (Proxy @Wallets.API) (Wallets.handlers logTrace)
    :<|> hoist' (Proxy @Accounts.API) (Accounts.handlers logTrace)
    :<|> hoist' (Proxy @Transactions.API) (Transactions.handlers logTrace pm txpConfig sendTx')
    :<|> hoist' (Proxy @Settings.API) Settings.handlers
    :<|> hoist' (Proxy @Info.API) (Info.handlers logTrace diffusion ntpStatus)
  where
    hoist'
        :: forall (api :: *). HasServer api '[]
        => Proxy api
        -> ServerT api MonadV1
        -> Server api
    hoist' p = hoistServer p naturalTransformation
    sendTx' = sendTx diffusion
