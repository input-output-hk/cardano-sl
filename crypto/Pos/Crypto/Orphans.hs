{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pos.Crypto.Orphans
       (
       ) where

import           Universum

import qualified Cardano.Crypto.Wallet as CC
import qualified Cardano.Crypto.Wallet.Encrypted as CC
import qualified Crypto.SCRAPE as Scrape
import           Crypto.Scrypt (EncryptedPass (..))
import qualified Crypto.Sign.Ed25519 as Ed25519
import           Data.Aeson (FromJSON (..), ToJSON (..))
import           Data.Hashable (Hashable)
import           Data.SafeCopy (base, deriveSafeCopySimple)
import           Serokell.Util.Base64 (JsonByteString (..))

import           Pos.Binary.Class (Bi (..), decodeBinary, encodeBinary)

instance Hashable Ed25519.PublicKey
instance Hashable Ed25519.SecretKey
instance Hashable Ed25519.Signature

instance NFData Ed25519.PublicKey
instance NFData Ed25519.SecretKey
instance NFData Ed25519.Signature

deriveSafeCopySimple 0 'base ''Ed25519.PublicKey
deriveSafeCopySimple 0 'base ''Ed25519.SecretKey
deriveSafeCopySimple 0 'base ''Ed25519.Signature

instance FromJSON Ed25519.PublicKey where
    parseJSON v = Ed25519.PublicKey . getJsonByteString <$> parseJSON v

instance ToJSON Ed25519.PublicKey where
    toJSON = toJSON . JsonByteString . Ed25519.openPublicKey

instance FromJSON Ed25519.Signature where
    parseJSON v = Ed25519.Signature . getJsonByteString <$> parseJSON v

instance ToJSON Ed25519.Signature where
    toJSON = toJSON . JsonByteString . Ed25519.unSignature

instance Bi Ed25519.PublicKey where
    encode (Ed25519.PublicKey k) = encode k
    decode = Ed25519.PublicKey <$> decode

instance Bi Ed25519.SecretKey where
    encode (Ed25519.SecretKey k) = encode k
    decode = Ed25519.SecretKey <$> decode

instance Bi Ed25519.Signature where
    encode (Ed25519.Signature s) = encode s
    decode = Ed25519.Signature <$> decode

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


