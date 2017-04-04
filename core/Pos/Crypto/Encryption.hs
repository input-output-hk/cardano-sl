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
import           Crypto.Hash         (Blake2b_256)
import qualified Data.Text.Encoding  as TE
import           Pos.Binary.Class    (Bi, encodeStrict)
import           Pos.Binary.Crypto   ()
import           Pos.Crypto.Hashing  (AbstractHash, unsafeAbstractHash)
import           Universum


----------------------------------------------------------------------------
-- Hashing
----------------------------------------------------------------------------

blake2b :: Bi a => a -> AbstractHash Blake2b_256 b
blake2b = unsafeAbstractHash

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
deriveAesKeyBS = AesKey . encodeStrict . blake2b

aesEncrypt :: ByteString -> AesKey -> Either CryptoError ByteString
aesEncrypt input (fromAESKey -> sk) = ctrCombine <$> init <*> pure nullIV <*> pure input
  where
    -- FIXME: return either here
    init :: Either CryptoError AES256
    init = eitherCryptoError $ cipherInit sk

aesDecrypt :: ByteString -> AesKey -> Either CryptoError ByteString
aesDecrypt = aesEncrypt -- encryption/decryption is symmetric
