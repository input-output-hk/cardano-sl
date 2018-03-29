
module Cardano.Wallet.WalletLayer.Kernel where

import Universum

import Cardano.Wallet.WalletLayer (PassiveWalletLayer (..))

kernelWalletLayer :: forall m. PassiveWalletLayer m
kernelWalletLayer = PassiveWalletLayer
    { pwlGetWalletAddresses  = error "Not implemented!"
    , pwlGetWalletMeta       = error "Not implemented!"
    }


