-- | Hierarchical derivation interface.
-- This module provides basic operations with HD wallets.
-- You can read HD wallets overall description in docs/hd.md.

module Pos.Crypto.HD
       ( HDPassphrase (..)
       , HDAddressPayload (..)
       , ShouldCheckPassphrase (..)
       , packHDAddressAttr
       , unpackHDAddressAttr
       , deriveHDPublicKey
       , deriveHDSecretKey
       , deriveHDPassphrase
       , decryptChaChaPoly
       , encryptChaChaPoly
       , toEither

       , firstHardened
       , firstNonHardened
       , isHardened
       ) where

import           Cardano.Crypto.Wallet (deriveXPrv, deriveXPub, unXPub)
import qualified Crypto.Cipher.ChaChaPoly1305 as C
import           Crypto.Error
import           Crypto.Hash (SHA512 (..))
import qualified Crypto.KDF.PBKDF2 as PBKDF2
import qualified Crypto.MAC.Poly1305 as Poly
import           Data.ByteArray as BA (convert)
import           Data.ByteString.Char8 as B
import           Universum

import           Pos.Binary.Class (Bi, decodeFull, serialize')
import           Pos.Crypto.Scrypt (EncryptedPass)
import           Pos.Crypto.Signing.Types (EncryptedSecretKey (..), PassPhrase, PublicKey (..),
                                           checkPassMatches)

-- | Passphrase is a hash of root public key.
data HDPassphrase = HDPassphrase !ByteString
    deriving Show

-- | HDAddressPayload consists of
--
-- * serialiazed and encrypted using HDPassphrase derivation path from the
-- root key to given descendant key (using ChaChaPoly1305 algorithm)
--
-- * cryptographic tag
--
-- For more information see 'packHDAddressAttr' and 'encryptChaChaPoly'.
data HDAddressPayload
    = HDAddressPayload
    { getHDAddressPayload :: !ByteString
    } deriving (Eq, Ord, Show, Generic)

instance NFData HDAddressPayload

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
-- Indices less than @firstHardened@ are non-hardened indices.
firstHardened :: Word32
firstHardened = 2 ^ (31 :: Word32)

firstNonHardened :: Word32
firstNonHardened = 0

isHardened :: Word32 -> Bool
isHardened = ( >= firstHardened)

-- | Derive public key from public key in non-hardened (normal) way.
-- If you try to pass an 'isHardened' index, error will be called.
deriveHDPublicKey :: PublicKey -> Word32 -> PublicKey
deriveHDPublicKey (PublicKey xpub) childIndex
    | isHardened childIndex =
        error "Wrong index for non-hardened derivation"
    | otherwise =
        maybe (error "deriveHDPublicKey: deriveXPub failed") PublicKey $
          deriveXPub xpub childIndex

-- | Whether to call @checkPassMatches@
newtype ShouldCheckPassphrase = ShouldCheckPassphrase Bool

-- | Derive child's secret key from parent's secret key using user's passphrase.
deriveHDSecretKey
    :: (Bi PassPhrase, Bi EncryptedPass)
    => ShouldCheckPassphrase
    -> PassPhrase
    -> EncryptedSecretKey
    -> Word32
    -> Maybe EncryptedSecretKey
deriveHDSecretKey (ShouldCheckPassphrase checkPass) passPhrase encSK@(EncryptedSecretKey xprv pph) childIndex = do
    when checkPass $ checkPassMatches passPhrase encSK
    pure $
        EncryptedSecretKey
            (deriveXPrv passPhrase xprv childIndex)
            pph

addrAttrNonce :: ByteString
addrAttrNonce = "serokellfore"

-- | Serialize tree path and encrypt it using HDPassphrase via ChaChaPoly1305.
packHDAddressAttr :: HDPassphrase -> [Word32] -> HDAddressPayload
packHDAddressAttr (HDPassphrase passphrase) path = do
    let !pathSer = serialize' path
    let !packCF =
          encryptChaChaPoly
              addrAttrNonce
              passphrase
              ""
              pathSer
    case packCF of
        CryptoFailed er -> error $ "Error in packHDAddressAttr: " <> show er
        CryptoPassed p  -> HDAddressPayload p

-- | Try to decrypt HDAddressPayload using HDPassphrase.
unpackHDAddressAttr :: HDPassphrase -> HDAddressPayload -> Maybe [Word32]
unpackHDAddressAttr (HDPassphrase passphrase) (HDAddressPayload payload) = do
    let !unpackCF =
          decryptChaChaPoly
              addrAttrNonce
              passphrase
              ""
              payload
    case unpackCF of
        Left _ -> Nothing
        Right p -> case decodeFull p of
            Left _ -> Nothing
            Right path -> pure path

-- | Take HDPassphrase as symmetric key and serialized derivation path
-- and encrypt it using ChaChaPoly1305 scheme.
encryptChaChaPoly
    :: ByteString -- Nonce (12 random bytes)
    -> ByteString -- Symmetric key (must be 32 bytes)
    -> ByteString -- Encryption header.
                  -- Header is chunk of data we want to transfer unecncrypted
                  -- but still want it to be part of tag digest.
                  -- So tag verifies validity of both encrypted data and unencrypted header.
    -> ByteString -- Input plaintext to be encrypted
    -> CryptoFailable ByteString -- Ciphertext with a 128-bit crypto-tag appended.
encryptChaChaPoly nonce key header plaintext = do
    st1 <- C.nonce12 nonce >>= C.initialize key
    let st2 = C.finalizeAAD $ C.appendAAD header st1
    let (out, st3) = C.encrypt plaintext st2
    let auth = C.finalize st3
    pure $ out <> BA.convert auth

toEither :: CryptoFailable a -> Either Text a
toEither (CryptoPassed x)  = pure x
toEither (CryptoFailed er) = Left $ show er

-- | Take HDPassphrase as symmetric key and encrypted derivation path (aka HDPayload)
-- and try to decrypt it.
-- Verify that appended crypto-tag is the same as got in result of work ChaChaPoly1305 algorithm.
decryptChaChaPoly
    :: ByteString -- Nonce (12 random bytes)
    -> ByteString -- Symmetric key
    -> ByteString -- Encryption header, optional associated data.
    -> ByteString -- Input plaintext to be decrypted
    -> Either Text ByteString -- Decrypted text or error
decryptChaChaPoly nonce key header encDataWithTag = do
    let tagSize = 16::Int
    let l = B.length encDataWithTag
    unless (l >= tagSize) $
        Left $ "Length of encrypted text must be at least " <> show tagSize
    let (encData, rawTag) = B.splitAt (l - 16) encDataWithTag
    tag <- toEither (Poly.authTag rawTag)
    st1 <- toEither (C.nonce12 nonce >>= C.initialize key)
    let st2 = C.finalizeAAD $ C.appendAAD header st1
    let (out, st3) = C.decrypt encData st2
    unless (C.finalize st3 == tag) $
        Left $ "Crypto-tag mismatch"
    -- is it free from mem leaks?
    pure out
