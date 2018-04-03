-- | Module for safe (zero-memory) signing.

module Pos.Crypto.Signing.Safe
       ( changeEncPassphrase
       , safeSign
       , safeToPublic
       , safeKeyGen
       , safeDeterministicKeyGen
       , withSafeSigner
       , withSafeSignerUnsafe
       , withSafeSigners
       , fakeSigner
       , safeCreateProxyCert
       , safeCreatePsk
       , createProxyCert
       , createPsk
       , module Pos.Crypto.Signing.Types.Safe
       ) where

import           Universum

import qualified Cardano.Crypto.Wallet as CC
import           Crypto.Random (MonadRandom, getRandomBytes)
import qualified Data.ByteString as BS
import           Data.Coerce (coerce)

import           Pos.Binary.Class (Bi, Raw)
import qualified Pos.Binary.Class as Bi
import           Pos.Crypto.Configuration (ProtocolMagic)
import           Pos.Crypto.Hashing (Hash, hash)
import qualified Pos.Crypto.Scrypt as S
import           Pos.Crypto.Signing.Signing (ProxyCert (..), ProxySecretKey (..), PublicKey (..),
                                             SecretKey (..), Signature (..), sign, toPublic)
import           Pos.Crypto.Signing.Tag (SignTag (SignProxySK), signTag)
import           Pos.Crypto.Signing.Types.Safe

-- | Regenerates secret key with new passphrase.
-- Note: This operation keeps corresponding public key and derived (child) keys unchanged.
changeEncPassphrase
    :: (Bi PassPhrase, MonadRandom m)
    => PassPhrase
    -> PassPhrase
    -> EncryptedSecretKey
    -> m (Maybe EncryptedSecretKey)
changeEncPassphrase oldPass newPass esk@(EncryptedSecretKey sk _)
    | isJust $ checkPassMatches oldPass esk =
        Just <$> mkEncSecretUnsafe newPass (CC.xPrvChangePass oldPass newPass sk)
    | otherwise = return Nothing

signRaw' :: ProtocolMagic
         -> Maybe SignTag
         -> PassPhrase
         -> EncryptedSecretKey
         -> ByteString
         -> Signature Raw
signRaw' pm mbTag (PassPhrase pp) (EncryptedSecretKey sk _) x =
    Signature (CC.sign pp sk (tag <> x))
  where
    tag = maybe mempty (signTag pm) mbTag

sign'
    :: (Bi a)
    => ProtocolMagic -> SignTag -> PassPhrase -> EncryptedSecretKey -> a -> Signature a
sign' pm t pp sk = coerce . signRaw' pm (Just t) pp sk . Bi.serialize'

safeCreateKeypairFromSeed
    :: BS.ByteString
    -> PassPhrase
    -> (CC.XPub, CC.XPrv)
safeCreateKeypairFromSeed seed (PassPhrase pp) =
    let prv = CC.generate seed pp
    in  (CC.toXPub prv, prv)

-- NB. It's recommended to run it with 'runSecureRandom' from
-- "Pos.Crypto.Random" because the OpenSSL generator is probably safer than
-- the default IO generator.
safeKeyGen
    :: (MonadRandom m, Bi PassPhrase, Bi (Hash ByteString))
    => PassPhrase -> m (PublicKey, EncryptedSecretKey)
safeKeyGen pp = do
    seed <- getRandomBytes 32
    pure $ safeDeterministicKeyGen seed pp

safeDeterministicKeyGen
    :: (Bi PassPhrase, Bi (Hash ByteString))
    => BS.ByteString
    -> PassPhrase
    -> (PublicKey, EncryptedSecretKey)
safeDeterministicKeyGen seed pp =
    bimap
        PublicKey
        (mkEncSecretWithSaltUnsafe (S.mkSalt (hash seed)) pp)
        (safeCreateKeypairFromSeed seed pp)

safeSign :: (Bi a) => ProtocolMagic -> SignTag -> SafeSigner -> a -> Signature a
safeSign pm t (SafeSigner sk pp) = sign' pm t pp sk
safeSign pm t (FakeSigner sk)    = sign  pm t sk

safeToPublic :: SafeSigner -> PublicKey
safeToPublic (SafeSigner sk _) = encToPublic sk
safeToPublic (FakeSigner sk)   = toPublic sk

-- | We can make SafeSigner only inside IO bracket, so
-- we can manually cleanup all IO buffers we use to store passphrase
-- (when we'll actually use them)
withSafeSigners
    :: (Monad m, Bi PassPhrase, Traversable t)
    => t EncryptedSecretKey
    -> m PassPhrase
    -> (t SafeSigner -> m a) -> m a
withSafeSigners sks ppGetter action = do
    pp <- ppGetter
    let mss = map (\sk -> SafeSigner sk pp) sks
    action mss

withSafeSigner
    :: (Monad m, Bi PassPhrase)
    => EncryptedSecretKey
    -> m PassPhrase
    -> (Maybe SafeSigner -> m a)
    -> m a
withSafeSigner sk ppGetter action = do
    pp <- ppGetter
    withSafeSigners (Identity sk) (pure pp) $
        action . (checkPassMatches pp sk $>) . runIdentity

-- This function is like @withSafeSigner@ but doesn't check @checkPassMatches@
withSafeSignerUnsafe
    :: (Monad m, Bi PassPhrase)
    => EncryptedSecretKey
    -> m PassPhrase
    -> (SafeSigner -> m a)
    -> m a
withSafeSignerUnsafe sk ppGetter action = do
    pp <- ppGetter
    withSafeSigners (Identity sk) (pure pp) $ action . runIdentity

-- | We need this to be able to perform signing with unencrypted `SecretKey`s,
-- where `SafeSigner` is required
fakeSigner :: SecretKey -> SafeSigner
fakeSigner = FakeSigner

-- | Proxy certificate creation from secret key of issuer, public key
-- of delegate and the message space ω.
safeCreateProxyCert :: (Bi w) => ProtocolMagic -> SafeSigner -> PublicKey -> w -> ProxyCert w
safeCreateProxyCert pm ss (PublicKey delegatePk) o = coerce $ ProxyCert sig
  where
    Signature sig = safeSign pm SignProxySK ss $
                      mconcat
                          ["00", CC.unXPub delegatePk, Bi.serialize' o]

-- | Creates proxy secret key
safeCreatePsk :: (Bi w) => ProtocolMagic -> SafeSigner -> PublicKey -> w -> ProxySecretKey w
safeCreatePsk pm ss delegatePk w =
    UnsafeProxySecretKey
        { pskOmega      = w
        , pskIssuerPk   = safeToPublic ss
        , pskDelegatePk = delegatePk
        , pskCert       = safeCreateProxyCert pm ss delegatePk w
        }

-- [CSL-1157] `createProxyCert` and `createProxySecretKey` are not safe and
--   left here because of their implementation details
--   in future should be removed completely, now left for compatibility with tests

-- | Proxy certificate creation from secret key of issuer, public key
-- of delegate and the message space ω.
createProxyCert :: (Bi w) => ProtocolMagic -> SecretKey -> PublicKey -> w -> ProxyCert w
createProxyCert pm = safeCreateProxyCert pm . fakeSigner

-- | Creates proxy secret key
createPsk :: (Bi w) => ProtocolMagic -> SecretKey -> PublicKey -> w -> ProxySecretKey w
createPsk pm = safeCreatePsk pm . fakeSigner
