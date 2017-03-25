-- | Hierarchical derivation interface.

module Pos.Crypto.HD
       ( HDPassphrase
       , HDAddressPayload (..)
       , packHDAddressAttr
       , deriveHDPublicKey
       , deriveHDSecretKey
       , deriveHDPassphrase
       ) where

import           Cardano.Crypto.Wallet        (deriveXPrv, deriveXPrvHardened, deriveXPub,
                                               unXPub)
import           Crypto.Cipher.ChaChaPoly1305 as C
import           Crypto.Error                 (CryptoFailable (..))
import           Crypto.Hash                  (SHA512 (..))
import qualified Crypto.KDF.PBKDF2            as PBKDF2
import           Data.ByteArray               as BA (ByteArrayAccess, convert)
import           Data.ByteString.Char8        as B
import           Universum

import           Pos.Binary.Class             (encodeStrict)
import           Pos.Crypto.Signing           (PublicKey (..), SecretKey (..))

-- | Passphrase is a hash of root public key.
-- We don't use root public key to store money, we use hash of it instead.
newtype HDPassphrase = HDPassphrase ByteString

-- | HDAddressPayload consists of
--
-- * serialiazed and encrypted with symmetric scheme path from the root
-- key to given descendant key with passphrase (via ChaChaPoly1305 algorithm)
--
-- * cryptographic tag
--
-- For more information see 'packHDAddressAttr' and 'encryptChaChaPoly'.
newtype HDAddressPayload = HDAddressPayload ByteString
    deriving (Eq, Ord, Show, NFData, Generic)

-- | Compute passphrase as hash of the root public key.
deriveHDPassphrase :: PublicKey -> HDPassphrase
deriveHDPassphrase (PublicKey pk) = HDPassphrase $
    PBKDF2.generate
        (PBKDF2.prfHMAC SHA512)
        (PBKDF2.Parameters
             500 -- Parameters for the hashing function. 500 iter of PBDKF2 with HMAC-SHA256
             passLen)
        (unXPub pk)
        ("address-hashing"::ByteString)
  where
    -- Password length in bytes
    passLen = 32

-- Direct children of node are numbered from 0 to 2^32-1.
-- Child with index less or equal @maxHardened@ is a hardened child.
maxHardened :: Word32
maxHardened = 2 ^ (31 :: Word32) - 1

-- | Derive public key from public key in non-hardened (normal) way.
-- If you try to pass index more than @maxHardened@, error will be called.
deriveHDPublicKey :: PublicKey -> Word32 -> PublicKey
deriveHDPublicKey (PublicKey xpub) childIndex
    -- Is it the best solution?
    | childIndex <= maxHardened = error "Wrong index for non-hardened derivation"
    | otherwise = PublicKey $ deriveXPub xpub (childIndex - maxHardened - 1)

-- | Derive secret key from secret key.
-- If @childIndex <= maxHardened@ key will be deriving hardened way, otherwise non-hardened.
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

-- | Serialize tree path and encrypt it using passphrase via ChaChaPoly1305.
packHDAddressAttr :: HDPassphrase -> [Word32] -> HDAddressPayload
packHDAddressAttr (HDPassphrase simkey) path = do
    let pathSer = encodeStrict path
    let packCF =
          encryptChaChaPoly
              "serokellfore"
              simkey
              "addressattr" -- I don't know what does mean this parameter.
                            -- Seems value isn't important...
              pathSer
    case packCF of
        CryptoFailed er -> error $ "Error during packHDAddressAttr: " <> show er
        CryptoPassed p  -> HDAddressPayload p

-- Wrapper around ChaChaPoly1305 module.
encryptChaChaPoly
    :: ByteString -- nonce (12 random bytes)
    -> ByteString -- symmetric key
    -> ByteString -- optional associated data (won't be encrypted)
    -> ByteString -- input plaintext to be encrypted
    -> CryptoFailable ByteString -- ciphertext with a 128-bit tag attached
encryptChaChaPoly nonce key header plaintext = do
    st1 <- C.nonce12 nonce >>= C.initialize key
    let st2 = C.finalizeAAD $ C.appendAAD header st1
    let (out, st3) = C.encrypt plaintext st2
    let auth = C.finalize st3
    pure $ out <> BA.convert auth
