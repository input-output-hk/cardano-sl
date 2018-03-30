module Cardano.Wallet.WalletLayer.Legacy
    ( legacyWalletLayer
    ) where

import           Universum

import           System.Wlog (usingLoggerName, logMessage)
import           Cardano.Wallet.WalletLayer (PassiveWalletLayer (..))

import           Cardano.Wallet.API.V1.Migration (migrate)

import           Pos.Wallet.Web.State.State (WalletDbReader, askWalletSnapshot, getWalletAddresses,
                                             getWalletMeta)

legacyWalletLayer
    :: forall ctx m. (WalletDbReader ctx m, MonadIO m, MonadThrow m)
    => PassiveWalletLayer m
legacyWalletLayer = PassiveWalletLayer
    { pwlGetWalletAddresses  = askWalletSnapshot >>= \ws -> migrate $ getWalletAddresses ws
    , pwlGetWalletMeta       = \cIdWal -> askWalletSnapshot >>= \ws -> pure $ getWalletMeta ws =<< (migrate cIdWal)
    , pwlWalletLogMessage    = \sev txt -> usingLoggerName "legacy-wallet" $ logMessage sev txt
    }

