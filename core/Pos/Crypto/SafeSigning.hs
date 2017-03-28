{-# LANGUAGE TemplateHaskell #-}

-- | Module for safe (zero-memory) signing

module Pos.Crypto.SafeSigning
       ( EncryptedSecretKey (..)
       , PassPhrase
       , SafeSigner
       , emptyPassphrase
       , noPassEncrypt
       , encToPublic
       , safeSign
       , safeToPublic
       , safeKeyGen
       , safeDeterministicKeyGen
       , withSafeSigner
       , fakeSigner
       ) where

import qualified Cardano.Crypto.Wallet as CC
import           Data.ByteArray        (ByteArray, ByteArrayAccess, ScrubbedBytes)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Lazy  as BSL
import           Data.Coerce           (coerce)
import           Data.SafeCopy         (base, deriveSafeCopySimple)
import           Data.Text.Buildable   (build)
import qualified Data.Text.Buildable   as B
import           Prelude               (show)
import           Universum             hiding (show)

import           Pos.Binary.Class      (Bi, Raw)
import qualified Pos.Binary.Class      as Bi
import           Pos.Crypto.Random     (secureRandomBS)
import           Pos.Crypto.Signing    (PublicKey (..), SecretKey (..), Signature (..),
                                        sign, toPublic)

newtype EncryptedSecretKey = EncryptedSecretKey CC.XPrv

instance Show EncryptedSecretKey where
    show _ = "<encrypted key>"

instance B.Buildable EncryptedSecretKey where
    build _ = "<encrypted key>"

newtype PassPhrase = PassPhrase ScrubbedBytes
    deriving (Eq, Ord, Monoid, NFData, ByteArray, ByteArrayAccess)

instance Show PassPhrase where
    show _ = "<passphrase>"

instance Buildable PassPhrase where
    build _ = "<passphrase>"

deriveSafeCopySimple 0 'base ''EncryptedSecretKey

-- | Empty passphrase used in development.
emptyPassphrase :: PassPhrase
emptyPassphrase = PassPhrase mempty

-- | Generate a public key using an encrypted secret key and passphrase
encToPublic :: EncryptedSecretKey -> PublicKey
encToPublic (EncryptedSecretKey sk) = PublicKey (CC.toXPub sk)

-- | Re-wrap unencrypted secret key as an encrypted one
noPassEncrypt :: SecretKey -> EncryptedSecretKey
noPassEncrypt (SecretKey k) = EncryptedSecretKey k

signRaw' :: PassPhrase -> EncryptedSecretKey -> ByteString -> Signature Raw
signRaw' (PassPhrase pp) (EncryptedSecretKey sk) x =
    Signature (CC.sign pp sk x)

sign' :: Bi a => PassPhrase -> EncryptedSecretKey -> a -> Signature a
sign' pp sk = coerce . signRaw' pp sk . BSL.toStrict . Bi.encode

safeCreateKeypairFromSeed
    :: BS.ByteString
    -> PassPhrase
    -> Maybe (CC.XPub, CC.XPrv)
safeCreateKeypairFromSeed seed (PassPhrase pp) = do
    prv <- CC.generate seed pp
    return (CC.toXPub prv, prv)

safeKeyGen :: MonadIO m => PassPhrase -> m (PublicKey, EncryptedSecretKey)
safeKeyGen pp = liftIO $ do
    seed <- secureRandomBS 32
    case safeCreateKeypairFromSeed seed pp of
        Nothing -> error "Pos.Crypto.SafeSigning.safeKeyGen:\
                         \ creating keypair from seed failed"
        Just (pk, sk) -> return (PublicKey pk, EncryptedSecretKey sk)

safeDeterministicKeyGen
    :: BS.ByteString
    -> PassPhrase
    -> Maybe (PublicKey, EncryptedSecretKey)
safeDeterministicKeyGen seed pp =
    bimap PublicKey EncryptedSecretKey <$> safeCreateKeypairFromSeed seed pp

-- | SafeSigner datatype to encapsulate sensible data
data SafeSigner = SafeSigner EncryptedSecretKey PassPhrase
                | FakeSigner SecretKey

safeSign :: Bi a => SafeSigner -> a -> Signature a
safeSign (SafeSigner sk pp) = sign' pp sk
safeSign (FakeSigner sk)    = sign sk

safeToPublic :: SafeSigner -> PublicKey
safeToPublic (SafeSigner sk _) = encToPublic sk
safeToPublic (FakeSigner sk)   = toPublic sk

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

-- | We need this to be able to perform signing with unencrypted `SecretKey`s,
-- where `SafeSigner` is required
fakeSigner :: SecretKey -> SafeSigner
fakeSigner = FakeSigner
