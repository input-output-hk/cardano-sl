{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Wrapper over AES. `encode` and `decode` use AES256 CTR mode with
-- IV = 0.
module Pos.Crypto.Encryption
       ( AesKey
       , deriveAesKey
       , deriveAesKeyBS
       , aesEncrypt
       , aesDecrypt
       ) where

import           Crypto.Cipher.AES   (AES256)
import           Crypto.Cipher.Types (BlockCipher (..), cipherInit, ctrCombine, nullIV)
import           Crypto.Error        (CryptoError, eitherCryptoError)
import           Crypto.Hash         (Blake2b_256, Digest, hash)
import           Data.ByteArray      (convert)
import qualified Data.Text.Encoding  as TE
import           Pos.Binary.Crypto   ()
import           Universum


----------------------------------------------------------------------------
-- Hashing
----------------------------------------------------------------------------

blake2b :: ByteString -> ByteString
blake2b = convert @(Digest Blake2b_256) . hash

----------------------------------------------------------------------------
-- AES
----------------------------------------------------------------------------

-- | Key to encrypt data
newtype AesKey = AesKey
    { fromAESKey :: ByteString
    } deriving (Show, Eq, Generic, Hashable)

deriveAesKey :: Text -> AesKey
deriveAesKey = deriveAesKeyBS . TE.encodeUtf8

deriveAesKeyBS :: ByteString -> AesKey
deriveAesKeyBS = AesKey . blake2b

aesEncrypt :: ByteString -> AesKey -> Either CryptoError ByteString
aesEncrypt input (fromAESKey -> sk) = ctrCombine <$> init <*> pure nullIV <*> pure input
  where
    -- FIXME: return either here
    init :: Either CryptoError AES256
    init = eitherCryptoError $ cipherInit sk

aesDecrypt :: ByteString -> AesKey -> Either CryptoError ByteString
aesDecrypt = aesEncrypt -- encryption/decryption is symmetric
