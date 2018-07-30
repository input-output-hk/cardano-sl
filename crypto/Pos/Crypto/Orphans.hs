{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pos.Crypto.Orphans
       (
       ) where

import           Universum

import qualified Cardano.Crypto.Wallet as CC
import qualified Cardano.Crypto.Wallet.Encrypted as CC
import           Codec.CBOR.Decoding (Decoder)
import qualified Codec.CBOR.Encoding as E
import           Crypto.Error (CryptoFailable (..))
import qualified Crypto.PubKey.Ed25519 as Ed25519
import qualified Crypto.SCRAPE as Scrape
import           Crypto.Scrypt (EncryptedPass (..))
import           Data.Aeson (FromJSON (..), ToJSON (..))
import           Data.Aeson.Types (Parser)
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import           Data.Hashable (Hashable)
import           Data.SafeCopy (base, deriveSafeCopySimple)
import           Data.SafeCopy (SafeCopy (..), contain, safeGet, safePut)
import qualified Data.Text as T
import           Serokell.Util.Base64 (JsonByteString (..))

import           Pos.Binary.Class (Bi (..), decodeBinary, encodeBinary)

instance Hashable Ed25519.PublicKey where
  hashWithSalt salt pk = hashWithSalt salt $ (BA.convert pk :: BS.ByteString)

instance Hashable Ed25519.SecretKey where
  hashWithSalt salt pk = hashWithSalt salt $ (BA.convert pk :: BS.ByteString)

instance Hashable Ed25519.Signature where
  hashWithSalt salt pk = hashWithSalt salt $ (BA.convert pk :: BS.ByteString)


instance Ord Ed25519.PublicKey where
  compare x1 x2 = compare (BA.convert x1 :: BS.ByteString) (BA.convert x2 :: BS.ByteString)

instance Ord Ed25519.SecretKey where
  compare x1 x2 = compare (BA.convert x1 :: BS.ByteString) (BA.convert x2 :: BS.ByteString)

instance Ord Ed25519.Signature where
  compare x1 x2 = compare (BA.convert x1 :: BS.ByteString) (BA.convert x2 :: BS.ByteString)


instance SafeCopy BA.Bytes where
  putCopy s =
    let
      toByteString :: BA.Bytes -> BS.ByteString
      toByteString = BA.convert
    in
      contain $ safePut (toByteString s)
  getCopy =
    let
      fromByteString :: BS.ByteString -> BA.Bytes
      fromByteString = BA.convert
    in
      contain $ fromByteString <$> safeGet


instance SafeCopy BA.ScrubbedBytes where
  putCopy s =
    let
      toByteString :: BA.ScrubbedBytes -> BS.ByteString
      toByteString = BA.convert
    in
      contain $ safePut (toByteString s)
  getCopy =
    let
      fromByteString :: BS.ByteString -> BA.ScrubbedBytes
      fromByteString = BA.convert
    in
      contain $ fromByteString <$> safeGet


deriveSafeCopySimple 0 'base ''Ed25519.PublicKey
deriveSafeCopySimple 0 'base ''Ed25519.SecretKey
deriveSafeCopySimple 0 'base ''Ed25519.Signature


instance FromJSON Ed25519.PublicKey where
  parseJSON v =
    let
      fromByteString :: BS.ByteString -> BA.Bytes
      fromByteString = BA.convert
      fromCryptoToAeson :: CryptoFailable a -> Parser a
      fromCryptoToAeson (CryptoFailed e) = error $ mappend "Pos.Crypto.Orphan.parseJSON Ed25519.PublicKey failed because " (T.pack $ show e)
      fromCryptoToAeson (CryptoPassed r) = return r
    in
      do
        res <- Ed25519.publicKey . fromByteString . getJsonByteString <$> parseJSON v
        fromCryptoToAeson res

instance ToJSON Ed25519.PublicKey where
  toJSON =
    let
      toByteString :: Ed25519.PublicKey -> BS.ByteString
      toByteString = BA.convert
    in
      toJSON . JsonByteString . toByteString

instance FromJSON Ed25519.Signature where
  parseJSON v =
    let
      fromByteString :: BS.ByteString -> BA.Bytes
      fromByteString = BA.convert
      fromCryptoToAeson :: CryptoFailable a -> Parser a
      fromCryptoToAeson (CryptoFailed e) = error $ mappend "Pos.Crypto.Orphan.parseJSON Ed25519.Signature failed because " (T.pack $ show e)
      fromCryptoToAeson (CryptoPassed r) = return r
    in
      do
        res <- Ed25519.signature . fromByteString . getJsonByteString <$> parseJSON v
        fromCryptoToAeson res

instance ToJSON Ed25519.Signature where
  toJSON =
    let
      toByteString :: Ed25519.Signature -> BS.ByteString
      toByteString = BA.convert
    in
      toJSON . JsonByteString . toByteString

instance Bi Ed25519.PublicKey where
    encode =  E.encodeBytes . BA.convert
    decode =
      let
        fromByteString :: BS.ByteString -> BA.Bytes
        fromByteString = BA.convert
        fromCryptoToAeson :: CryptoFailable a -> Decoder s a
        fromCryptoToAeson (CryptoFailed e) = error $ mappend "Pos.Crypto.Orphan.decode Ed25519.PublicKey failed because " (T.pack $ show e)
        fromCryptoToAeson (CryptoPassed r) = return r
      in
        do
          res <- Ed25519.publicKey . fromByteString <$> decode
          fromCryptoToAeson res

instance Bi Ed25519.SecretKey where
    encode =  E.encodeBytes . BA.convert
    decode =
      let
        fromByteString :: BS.ByteString -> BA.Bytes
        fromByteString = BA.convert
        fromCryptoToAeson :: CryptoFailable a -> Decoder s a
        fromCryptoToAeson (CryptoFailed e) = error $ mappend "Pos.Crypto.Orphan.decode Ed25519.SecretKey failed because " (T.pack $ show e)
        fromCryptoToAeson (CryptoPassed r) = return r
      in
        do
          res <- Ed25519.secretKey . fromByteString <$> decode
          fromCryptoToAeson res

instance Bi Ed25519.Signature where
    encode =  E.encodeBytes . BA.convert
    decode =
      let
        fromByteString :: BS.ByteString -> BA.Bytes
        fromByteString = BA.convert
        fromCryptoToAeson :: CryptoFailable a -> Decoder s a
        fromCryptoToAeson (CryptoFailed e) = error $ mappend "Pos.Crypto.Orphan.decode Ed25519.Signature failed because " (T.pack $  show e)
        fromCryptoToAeson (CryptoPassed r) = return r
      in
        do
          res <- Ed25519.signature . fromByteString <$> decode
          fromCryptoToAeson res

----------------------------------------------------------------------------
-- Bi instances for Scrape
----------------------------------------------------------------------------

instance Bi Scrape.PublicKey where
    encode = encodeBinary
    decode = decodeBinary

instance Bi Scrape.KeyPair where
    encode = encodeBinary
    decode = decodeBinary

instance Bi Scrape.Secret where
    encode = encodeBinary
    decode = decodeBinary

instance Bi Scrape.DecryptedShare where
    encode = encodeBinary
    decode = decodeBinary

instance Bi Scrape.EncryptedSi where
    encode = encodeBinary
    decode = decodeBinary

instance Bi Scrape.ExtraGen where
    encode = encodeBinary
    decode = decodeBinary

instance Bi Scrape.Commitment where
    encode = encodeBinary
    decode = decodeBinary

instance Bi Scrape.Proof where
    encode = encodeBinary
    decode = decodeBinary

instance Bi Scrape.ParallelProofs where
    encode = encodeBinary
    decode = decodeBinary

instance Bi EncryptedPass where
    encode (EncryptedPass ep) = encode ep
    decode = EncryptedPass <$> decode


deriveSafeCopySimple 0 'base ''CC.ChainCode
deriveSafeCopySimple 0 'base ''CC.EncryptedKey

deriveSafeCopySimple 0 'base ''CC.XSignature
deriveSafeCopySimple 0 'base ''CC.XPub
deriveSafeCopySimple 0 'base ''CC.XPrv
