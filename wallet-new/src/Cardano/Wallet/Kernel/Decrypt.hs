module Cardano.Wallet.Kernel.Decrypt
    ( decryptAddress
    , decryptHdLvl2DerivationPath
    , keyToWalletDecrCredentials
    , WalletDecrCredentials
    , WalletDecrCredentialsKey(..)
    ) where

import           Universum

import           Data.List ((!!))

import           Pos.Wallet.Web.Tracking.Decrypt

import           Cardano.Wallet.Kernel.DB.HdWallet (HdAccountIx (..),
                     HdAddressIx (..))
import           Pos.Core (Address, aaPkDerivationPath, addrAttributesUnwrapped)
import           Pos.Crypto (HDPassphrase, unpackHDAddressAttr)


decryptHdLvl2DerivationPath :: HDPassphrase
                            -> Address
                            -> Maybe (HdAccountIx, HdAddressIx)
decryptHdLvl2DerivationPath hdPass addr = do
    hdPayload <- aaPkDerivationPath $ addrAttributesUnwrapped addr
    derPath <- unpackHDAddressAttr hdPass hdPayload
    guard $ length derPath == 2
    pure (HdAccountIx (derPath !! 0), HdAddressIx (derPath !! 1))
