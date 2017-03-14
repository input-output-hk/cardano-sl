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

import           Cardano.Crypto.Wallet (deriveXPrv, deriveXPrvHardened, deriveXPub)
import           Pos.Crypto.Signing    (PublicKey (..), SecretKey (..))

newtype HDPassphrase = HDPassphrase ByteString

newtype HDAddressPayload = HDAddressPayload ByteString
    deriving (Eq, Ord, Show, NFData, Generic)

maxHardened :: Word32
maxHardened = fromIntegral $ (2::Word32)^(31::Word32)-1

deriveHDPublicKey :: PublicKey -> Word32 -> PublicKey
deriveHDPublicKey (PublicKey xpub) childIndex
    -- Is it the best solution?
    | childIndex <= maxHardened = error "Wrong index for non-hardened derivation"
    | otherwise = PublicKey $ deriveXPub xpub (childIndex - maxHardened - 1)

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
