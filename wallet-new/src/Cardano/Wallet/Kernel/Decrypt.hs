module Cardano.Wallet.Kernel.Decrypt
    ( decryptHdLvl2DerivationPath
    , eskToHdPassphrase
    , selectOwnAddresses
    ) where

import           Universum

import           Data.List ((!!))

import           Cardano.Wallet.Kernel.DB.HdWallet (HdAccountIx (..),
                     HdAddressIx (..))
import           Pos.Core (Address, aaPkDerivationPath, addrAttributesUnwrapped)
import           Pos.Crypto (EncryptedSecretKey, HDPassphrase,
                     deriveHDPassphrase, encToPublic, unpackHDAddressAttr)

selectOwnAddresses
    :: HDPassphrase
    -> (a -> Address)
    -> [a]
    -> [(a, (HdAccountIx, HdAddressIx))]
selectOwnAddresses hdPass getAddr =
    mapMaybe (\a -> (a,) <$> decryptHdLvl2DerivationPath hdPass (getAddr a))

decryptHdLvl2DerivationPath :: HDPassphrase
                            -> Address
                            -> Maybe (HdAccountIx, HdAddressIx)
decryptHdLvl2DerivationPath hdPass addr = do
    hdPayload <- aaPkDerivationPath $ addrAttributesUnwrapped addr
    derPath <- unpackHDAddressAttr hdPass hdPayload
    guard $ length derPath == 2
    pure (HdAccountIx (derPath !! 0), HdAddressIx (derPath !! 1))

eskToHdPassphrase :: EncryptedSecretKey -> HDPassphrase
eskToHdPassphrase = deriveHDPassphrase . encToPublic
