{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pos.Crypto.Orphans
       (
       ) where

import           Universum

import qualified Cardano.Crypto.Wallet as CC
import qualified Cardano.Crypto.Wallet.Encrypted as CC
import qualified Codec.CBOR.Encoding as E
import           Crypto.Error (CryptoFailable (..))
import qualified Crypto.PubKey.Ed25519 as Ed25519
import qualified Crypto.SCRAPE as Scrape
import           Crypto.Scrypt (EncryptedPass (..))
import           Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import           Data.Hashable (Hashable)
import           Data.SafeCopy (SafeCopy (..), base, contain,
                     deriveSafeCopySimple, safeGet, safePut)
import qualified Data.Text as T
import           Serokell.Util.Base64 (JsonByteString (..))

import           Pos.Binary.Class (Bi (..), decodeBinary, encodeBinary)


fromByteStringToBytes :: BS.ByteString -> BA.Bytes
fromByteStringToBytes = BA.convert

fromByteStringToScrubbedBytes :: BS.ByteString -> BA.ScrubbedBytes
fromByteStringToScrubbedBytes = BA.convert

toByteString :: (BA.ByteArrayAccess bin) => bin -> BS.ByteString
toByteString = BA.convert


instance Hashable Ed25519.PublicKey where
    hashWithSalt salt pk = hashWithSalt salt $ toByteString pk

instance Hashable Ed25519.SecretKey where
    hashWithSalt salt pk = hashWithSalt salt $ toByteString pk

instance Hashable Ed25519.Signature where
    hashWithSalt salt pk = hashWithSalt salt $ toByteString pk


instance Ord Ed25519.PublicKey where
    compare x1 x2 = compare (toByteString x1) (toByteString x2)

instance Ord Ed25519.SecretKey where
    compare x1 x2 = compare (toByteString x1) (toByteString x2)

instance Ord Ed25519.Signature where
    compare x1 x2 = compare (toByteString x1) (toByteString x2)



instance SafeCopy BA.Bytes where
    putCopy s = contain $ safePut (toByteString s)
    getCopy = contain $ fromByteStringToBytes <$> safeGet

instance SafeCopy BA.ScrubbedBytes where
    putCopy s = contain $ safePut (toByteString s)
    getCopy = contain $ fromByteStringToScrubbedBytes <$> safeGet


deriveSafeCopySimple 0 'base ''Ed25519.PublicKey
deriveSafeCopySimple 0 'base ''Ed25519.SecretKey
deriveSafeCopySimple 0 'base ''Ed25519.Signature


fromCryptoFailable :: MonadFail m => T.Text -> CryptoFailable a -> m a
fromCryptoFailable item (CryptoFailed e) = fail $ T.unpack $ "Pos.Crypto.Orphan." <> item <> " failed because " <> show e
fromCryptoFailable _ (CryptoPassed r) = return r


instance FromJSON Ed25519.PublicKey where
    parseJSON v = do
        res <- Ed25519.publicKey . fromByteStringToBytes . getJsonByteString <$> parseJSON v
        fromCryptoFailable "parseJSON Ed25519.PublicKey" res

instance ToJSON Ed25519.PublicKey where
    toJSON = toJSON . JsonByteString . toByteString

instance FromJSON Ed25519.Signature where
    parseJSON v = do
        res <- Ed25519.signature . fromByteStringToBytes . getJsonByteString <$> parseJSON v
        fromCryptoFailable "parseJSON Ed25519.Signature" res

instance ToJSON Ed25519.Signature where
    toJSON = toJSON . JsonByteString . toByteString



instance Bi Ed25519.PublicKey where

    encode =  E.encodeBytes . toByteString
    decode = do
        res <- Ed25519.publicKey . fromByteStringToBytes <$> decode
        fromCryptoFailable "decode Ed25519.PublicKey" res

instance Bi Ed25519.SecretKey where
    encode sk = E.encodeBytes $ BS.append (toByteString sk) (toByteString $ Ed25519.toPublic sk)
    decode = do
        res <- Ed25519.secretKey . fromByteStringToScrubbedBytes . BS.take Ed25519.secretKeySize <$> decode
        fromCryptoFailable "decode Ed25519.SecretKey" res

instance Bi Ed25519.Signature where
    encode =  E.encodeBytes . toByteString
    decode = do
        res <- Ed25519.signature . fromByteStringToBytes <$> decode
        fromCryptoFailable "decode Ed25519.Signature" res

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
