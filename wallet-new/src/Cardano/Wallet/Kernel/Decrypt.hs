module Cardano.Wallet.Kernel.Decrypt
    ( decryptAddress
    , keyToWalletDecrCredentials
    , WalletDecrCredentials
    , WalletDecrCredentialsKey(..)
    ) where

import           Universum

import           Data.List ((!!))

import           Pos.Wallet.Web.Tracking.Decrypt

import           Pos.Core (Address, aaPkDerivationPath, addrAttributesUnwrapped)
import           Pos.Crypto (HDPassphrase, unpackHDAddressAttr)
