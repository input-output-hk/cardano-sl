{-# LANGUAGE TemplateHaskell #-}

-- | Module for safe (zero-memory) signing

module Pos.Crypto.SafeSigning
       ( EncryptedSecretKey (..)
       , PassPhrase
       , SafeSigner
       , safeSign
       , withSafeSigner
       ) where

import qualified Cardano.Crypto.Wallet   as CC
import qualified Crypto.ECC.Edwards25519 as Ed25519
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Lazy    as BSL
import           Data.Coerce             (coerce)
import qualified Data.Text.Buildable     as B
import           Formatting              (build)
import           Prelude                 (show)
import           Universum               hiding (show)

import           Pos.Binary.Class        (Bi)
import qualified Pos.Binary.Class        as Bi
import           Pos.Crypto.Signing      (PublicKey)
import           Pos.Util.Binary         (Raw)

newtype EncryptedSecretKey = EncryptedSecretKey CC.XPrv
    deriving (Eq, NFData)

instance Show EncryptedSecretKey where
    show _ = "<encrypted key>"

instance B.Buildable EncryptedSecretKey where
    build _ = build ("<encrypted key>" :: Text)

-- | TODO This newtype should be eventually changed to wrapper
-- over `ScrubbedBytes`, after `cardano-crypto` support it.
newtype PassPhrase = PassPhrase CC.PassPhrase
    deriving (Eq)

deriveSafeCopySimple 0 'base ''EncryptedSecretKey

-- | Generate a public key using an encrypted secret key and passphrase
toPublic' :: PassPhrase -> EncryptedSecretKey -> PublicKey
toPublic' (PassPhrase pp) (EncryptedSecretKey sk) =
    PublicKey (CC.toXPub pp sk)

signRaw' :: PassPhrase -> EncryptedSecretKey -> ByteString -> Signature Raw
signRaw' (PassPhrase pp) (EncryptedSecretKey sk) x =
    Signature (CC.sign pp sk x)

sign' :: Bi a => PassPhrase -> EncryptedSecretKey -> a -> Signature a
sign' pp sk = coerce . signRaw' pp sk . BSL.toStrict . Bi.encode

-- | SafeSigner datatype to encapsulate sensible data
data SafeSigner = SafeSigner
    { ssSecretKey  :: EncryptedSecretKey
    , ssPassphrase :: PassPhrase
    }

safeSign :: Bi a => SafeSigner -> a -> Signature a
safeSign (SafeSigner sk pp) = sign' pp sk

safeToPublic :: SafeSigner -> PublicKey
safeToPublic (SafeSigner sk pp) = toPublic' pp sk

-- | We can make SafeSigner only inside IO bracket, so
-- we can manually cleanup all IO buffers we use to store passphrase
-- (when we'll actually use them)
withSafeSigner
    :: MonadIO m
    => EncryptedSecretKey
    -> m PassPhrase
    -> (SafeSigner -> m a)
    -> m a
withSafeSigner sk ppGetter action = ppGetter >>= action . SafeSigner sk
