-- | Hierarchical derivation interface.

module Pos.Crypto.HD
       ( HDPassphrase
       , HDAddressPayload (..)
       , packHDAddressAttr
       , deriveHDPublicKey
       , deriveHDSecretKey
       ) where

import           Data.ByteArray        (ByteArrayAccess)
import           Universum

import           Cardano.Crypto.Wallet (deriveXPrv, deriveXPrvHardened)
import           Pos.Crypto.Signing    (PublicKey (..), SecretKey (..))

newtype HDPassphrase = HDPassphrase ByteString

newtype HDAddressPayload = HDAddressPayload ByteString
    deriving (Eq, Ord, Show, NFData, Generic)

maxHardened :: Word32
maxHardened = fromIntegral $ (2::Word32)^(31::Word32)-1

deriveHDPublicKey :: PublicKey -> [Word32] -> Word32 -> PublicKey
deriveHDPublicKey parent parentPath childIndex
    | childIndex <= maxHardened = undefined --panic "Wrong index for non-hardened derivation"
    | otherwise = undefined -- TODO implement

deriveHDSecretKey :: ByteArrayAccess passPhrase
                  => passPhrase
                  -> SecretKey
                  -> Word32
                  -> SecretKey
deriveHDSecretKey passPhrase (SecretKey xprv) childIndex
  | childIndex <= maxHardened =
      SecretKey $ deriveXPrvHardened passPhrase xprv childIndex
  | otherwise =
      SecretKey $ deriveXPrv passPhrase xprv (childIndex - maxHardened - 1)

packHDAddressAttr :: HDPassphrase -> [Word32] -> HDAddressPayload
packHDAddressAttr = undefined -- TODO implement
