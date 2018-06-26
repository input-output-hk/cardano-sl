-- | Wrapper over AES. `encode` and `decode` use AES256 CTR mode with
-- IV = 0.
-- Decryption functions are used in wallet. Encryption is not used anywhere.

module Pos.Crypto.Encryption
       ( AesKey(..)
       , aesEncrypt
       , aesDecrypt
       ) where

import           Universum hiding (init)

import           Crypto.Cipher.AES (AES256)
import           Crypto.Cipher.Types (BlockCipher (..), cipherInit, ctrCombine, nullIV)
import           Crypto.Error (CryptoError, eitherCryptoError)


----------------------------------------------------------------------------
-- AES
----------------------------------------------------------------------------

-- | Key to encrypt data
newtype AesKey = AesKey
    { fromAESKey :: ByteString
    } deriving (Show, Eq, Generic, Hashable)

aesEncrypt :: ByteString -> AesKey -> Either CryptoError ByteString
aesEncrypt input (fromAESKey -> sk) = ctrCombine
    <$> init
    <*> pure nullIV
    <*> pure input
  where
    -- FIXME: return either here
    init :: Either CryptoError AES256
    init = eitherCryptoError $ cipherInit sk

aesDecrypt :: ByteString -> AesKey -> Either CryptoError ByteString
aesDecrypt = aesEncrypt -- encryption/decryption is symmetric
