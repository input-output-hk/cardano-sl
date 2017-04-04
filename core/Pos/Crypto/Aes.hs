{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Wrapper over AES. `encode` and `decode` use AES256 CTR mode with
-- IV = 0.
module Pos.Crypto.Aes
       ( AesKey
       , deriveAesKey
       , deriveAesKeyBS
       , aesEncrypt
       , aesDecrypt
       ) where

import           Crypto.Cipher.AES   (AES256)
import           Crypto.Cipher.Types (BlockCipher (..), cipherInit, ctrCombine, nullIV)
import           Crypto.Error        (eitherCryptoError)
import           Crypto.Hash         (Blake2b_256, Digest, HashAlgorithm, hash)
import           Data.ByteArray      (convert)
import qualified Data.Text.Encoding  as TE
import           Universum


----------------------------------------------------------------------------
-- Hashing
----------------------------------------------------------------------------

vhash :: forall x. (HashAlgorithm x) => Proxy x -> ByteString -> ByteString
vhash _ bs = convert (hash bs :: Digest x)

blake2b :: ByteString -> ByteString
blake2b = vhash (Proxy @Blake2b_256)

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

aesEncrypt :: ByteString -> AesKey -> ByteString
aesEncrypt input (fromAESKey -> sk) = ctrCombine init nullIV input
  where
    -- FIXME: return either here
    init :: AES256
    init = either (error . show) identity $ eitherCryptoError $ cipherInit sk

aesDecrypt :: ByteString -> AesKey -> ByteString
aesDecrypt = aesEncrypt -- encryption/decryption is symmetric
