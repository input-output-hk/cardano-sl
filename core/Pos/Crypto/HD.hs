-- | Hierarchical derivation interface.

module Pos.Crypto.HD
       ( HDPassphrase
       , HDAddressPayload (..)
       , packHDAddressAttr
       , deriveHDPublicKey
       , deriveHDSecretKey
       ) where

import           Universum

import           Pos.Crypto.Signing (PublicKey (..), SecretKey (..))

newtype HDPassphrase = HDPassphrase ByteString

newtype HDAddressPayload = HDAddressPayload ByteString
    deriving (Eq, Ord, Show, NFData, Generic)

deriveHDPublicKey :: PublicKey -> [Word32] -> Word32 -> PublicKey
deriveHDPublicKey parent parentPath childIndex
    | childIndex <= 2^31 - 1 = undefined --panic "Wrong index for non-hardened derivation"
    | otherwise = undefined -- TODO implement

deriveHDSecretKey :: SecretKey -> [Word32] -> Word32 -> SecretKey
deriveHDSecretKey parent parentPath childIndex = undefined -- TODO implement

packHDAddressAttr :: HDPassphrase -> [Word32] -> HDAddressPayload
packHDAddressAttr = undefined -- TODO implement
