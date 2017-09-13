-- | Module for safe (zero-memory) signing

module Pos.Crypto.Signing.Safe
       ( noPassEncrypt
       , checkPassMatches
       , changeEncPassphrase
       , safeSign
       , safeToPublic
       , safeKeyGen
       , safeDeterministicKeyGen
       , withSafeSigner
       , withSafeSigners
       , fakeSigner
       , safeCreateProxyCert
       , safeCreatePsk
       , createProxyCert
       , createPsk
       , module Pos.Crypto.Signing.Types.Safe
       ) where

import qualified Cardano.Crypto.Wallet as CC
import           Crypto.Random         (MonadRandom, getRandomBytes)
import qualified Data.ByteString       as BS
import           Data.Coerce           (coerce)
import           Universum

import           Pos.Binary.Class      (Bi, Raw)
import qualified Pos.Binary.Class      as Bi
import           Pos.Core.Configuration.Protocol (HasProtocolConstants)
import           Pos.Crypto.Hashing    (hash)
import           Pos.Crypto.Signing.Signing (ProxyCert (..), ProxySecretKey (..),
                                             PublicKey (..), SecretKey (..), Signature (..),
                                             sign, toPublic)
import           Pos.Crypto.Signing.Tag (SignTag (SignProxySK), signTag)
import           Pos.Crypto.Signing.Types.Safe

mkEncSecret :: Bi PassPhrase => PassPhrase -> CC.XPrv -> EncryptedSecretKey
mkEncSecret pp payload = EncryptedSecretKey payload (hash pp)

-- | Re-wrap unencrypted secret key as an encrypted one
noPassEncrypt :: Bi PassPhrase => SecretKey -> EncryptedSecretKey
noPassEncrypt (SecretKey k) = mkEncSecret emptyPassphrase k

checkPassMatches :: (Bi PassPhrase, Alternative f) => PassPhrase -> EncryptedSecretKey -> f ()
checkPassMatches pp (EncryptedSecretKey _ pph) = guard (hash pp == pph)

changeEncPassphrase
    :: Bi PassPhrase
    => PassPhrase
    -> PassPhrase
    -> EncryptedSecretKey
    -> Maybe EncryptedSecretKey
changeEncPassphrase oldPass newPass esk@(EncryptedSecretKey sk _) = do
    checkPassMatches oldPass esk
    return $ mkEncSecret newPass $ CC.xPrvChangePass oldPass newPass sk

signRaw' :: HasProtocolConstants
         => Maybe SignTag
         -> PassPhrase
         -> EncryptedSecretKey
         -> ByteString
         -> Signature Raw
signRaw' mbTag (PassPhrase pp) (EncryptedSecretKey sk _) x =
    Signature (CC.sign pp sk (tag <> x))
  where
    tag = maybe mempty signTag mbTag

sign'
    :: (HasProtocolConstants, Bi a)
    => SignTag -> PassPhrase -> EncryptedSecretKey -> a -> Signature a
sign' t pp sk = coerce . signRaw' (Just t) pp sk . Bi.serialize'

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
    :: (MonadRandom m, Bi PassPhrase)
    => PassPhrase -> m (PublicKey, EncryptedSecretKey)
safeKeyGen pp = do
    seed <- getRandomBytes 32
    pure $ safeDeterministicKeyGen seed pp

safeDeterministicKeyGen
    :: Bi PassPhrase
    => BS.ByteString
    -> PassPhrase
    -> (PublicKey, EncryptedSecretKey)
safeDeterministicKeyGen seed pp =
    bimap PublicKey (mkEncSecret pp) (safeCreateKeypairFromSeed seed pp)

safeSign :: (HasProtocolConstants, Bi a) => SignTag -> SafeSigner -> a -> Signature a
safeSign t (SafeSigner sk pp) = sign' t pp sk
safeSign t (FakeSigner sk)    = sign t sk

safeToPublic :: SafeSigner -> PublicKey
safeToPublic (SafeSigner sk _) = encToPublic sk
safeToPublic (FakeSigner sk)   = toPublic sk

-- | We can make SafeSigner only inside IO bracket, so
-- we can manually cleanup all IO buffers we use to store passphrase
-- (when we'll actually use them)
withSafeSigners
    :: (MonadIO m, Bi PassPhrase, Traversable t)
    => t EncryptedSecretKey
    -> m PassPhrase
    -> (Maybe (t SafeSigner) -> m a) -> m a
withSafeSigners sks ppGetter action = do
    pp <- ppGetter
    let mss = forM sks $ \sk -> checkPassMatches pp sk $> SafeSigner sk pp
    action mss

withSafeSigner
    :: (MonadIO m, Bi PassPhrase)
    => EncryptedSecretKey
    -> m PassPhrase
    -> (Maybe SafeSigner -> m a)
    -> m a
withSafeSigner sk ppGetter action =
    withSafeSigners (Identity sk) ppGetter (action . fmap runIdentity)

-- | We need this to be able to perform signing with unencrypted `SecretKey`s,
-- where `SafeSigner` is required
fakeSigner :: SecretKey -> SafeSigner
fakeSigner = FakeSigner

-- | Proxy certificate creation from secret key of issuer, public key
-- of delegate and the message space ω.
safeCreateProxyCert :: (HasProtocolConstants, Bi w) => SafeSigner -> PublicKey -> w -> ProxyCert w
safeCreateProxyCert ss (PublicKey delegatePk) o = coerce $ ProxyCert sig
  where
    Signature sig = safeSign SignProxySK ss $
                      mconcat
                          ["00", CC.unXPub delegatePk, Bi.serialize' o]

-- | Creates proxy secret key
safeCreatePsk :: (HasProtocolConstants, Bi w) => SafeSigner -> PublicKey -> w -> ProxySecretKey w
safeCreatePsk ss delegatePk w =
    ProxySecretKey w (safeToPublic ss) delegatePk $ safeCreateProxyCert ss delegatePk w

-- [CSL-1157] `createProxyCert` and `createProxySecretKey` are not safe and
--   left here because of their implementation details
--   in future should be removed completely, now left for compatibility with tests

-- | Proxy certificate creation from secret key of issuer, public key
-- of delegate and the message space ω.
createProxyCert :: (HasProtocolConstants, Bi w) => SecretKey -> PublicKey -> w -> ProxyCert w
createProxyCert = safeCreateProxyCert . fakeSigner

-- | Creates proxy secret key
createPsk :: (HasProtocolConstants, Bi w) => SecretKey -> PublicKey -> w -> ProxySecretKey w
createPsk = safeCreatePsk . fakeSigner
