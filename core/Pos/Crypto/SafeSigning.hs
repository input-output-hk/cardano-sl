-- | Module for safe (zero-memory) signing

module Pos.Crypto.SafeSigning
       ( EncryptedSecretKey (..)
       , PassPhrase
       , SafeSigner
       , passphraseLength
       , emptyPassphrase
       , noPassEncrypt
       , checkPassMatches
       , changeEncPassphrase
       , encToPublic
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
       ) where

import qualified Cardano.Crypto.Wallet as CC
import           Data.ByteArray        (ByteArray, ByteArrayAccess, ScrubbedBytes)
import qualified Data.ByteString       as BS
import           Data.Coerce           (coerce)
import           Data.Default          (Default (..))
import           Data.Text.Buildable   (build)
import qualified Data.Text.Buildable   as B
import qualified Prelude
import           Universum

import           Pos.Binary.Class      (Bi, Raw)
import qualified Pos.Binary.Class      as Bi
import           Pos.Crypto.Hashing    (Hash, hash)
import           Pos.Crypto.Random     (secureRandomBS)
import           Pos.Crypto.Signing    (ProxyCert (..), ProxySecretKey (..),
                                        PublicKey (..), SecretKey (..), Signature (..),
                                        sign, toPublic)
import           Pos.Crypto.SignTag    (SignTag (SignProxySK), signTag)

data EncryptedSecretKey = EncryptedSecretKey
    { eskPayload :: !CC.XPrv
    , eskHash    :: !(Hash PassPhrase)
    }

instance Show EncryptedSecretKey where
    show _ = "<encrypted key>"

instance B.Buildable EncryptedSecretKey where
    build _ = "<encrypted key>"

newtype PassPhrase = PassPhrase ScrubbedBytes
    deriving (Eq, Ord, Monoid, NFData, ByteArray, ByteArrayAccess)

passphraseLength :: Int
passphraseLength = 32

instance Show PassPhrase where
    show _ = "<passphrase>"

instance Buildable PassPhrase where
    build _ = "<passphrase>"

-- | Empty passphrase used in development.
emptyPassphrase :: PassPhrase
emptyPassphrase = PassPhrase mempty

instance Default PassPhrase where
    def = emptyPassphrase

{-instance Monoid PassPhrase where
    mempty = PassPhrase mempty
    mappend (PassPhrase p1) (PassPhrase p2) = PassPhrase (p1 `mappend` p2)-}

mkEncSecret :: Bi PassPhrase => PassPhrase -> CC.XPrv -> EncryptedSecretKey
mkEncSecret pp payload = EncryptedSecretKey payload (hash pp)

-- | Generate a public key using an encrypted secret key and passphrase
encToPublic :: EncryptedSecretKey -> PublicKey
encToPublic (EncryptedSecretKey sk _) = PublicKey (CC.toXPub sk)

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

signRaw' :: Maybe SignTag
         -> PassPhrase
         -> EncryptedSecretKey
         -> ByteString
         -> Signature Raw
signRaw' mbTag (PassPhrase pp) (EncryptedSecretKey sk _) x =
    Signature (CC.sign pp sk (tag <> x))
  where
    tag = maybe mempty signTag mbTag

sign'
    :: Bi a
    => SignTag -> PassPhrase -> EncryptedSecretKey -> a -> Signature a
sign' t pp sk = coerce . signRaw' (Just t) pp sk . Bi.serialize'

safeCreateKeypairFromSeed
    :: BS.ByteString
    -> PassPhrase
    -> (CC.XPub, CC.XPrv)
safeCreateKeypairFromSeed seed (PassPhrase pp) =
    let prv = CC.generate seed pp
    in  (CC.toXPub prv, prv)

safeKeyGen
    :: (MonadIO m, Bi PassPhrase)
    => PassPhrase -> m (PublicKey, EncryptedSecretKey)
safeKeyGen pp = liftIO $ do
    seed <- secureRandomBS 32
    let (pk, sk) = safeCreateKeypairFromSeed seed pp
    return (PublicKey pk, mkEncSecret pp sk)

safeDeterministicKeyGen
    :: Bi PassPhrase
    => BS.ByteString
    -> PassPhrase
    -> (PublicKey, EncryptedSecretKey)
safeDeterministicKeyGen seed pp =
    bimap PublicKey (mkEncSecret pp) (safeCreateKeypairFromSeed seed pp)

-- | SafeSigner datatype to encapsulate sensible data
data SafeSigner = SafeSigner EncryptedSecretKey PassPhrase
                | FakeSigner SecretKey

safeSign :: Bi a => SignTag -> SafeSigner -> a -> Signature a
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
safeCreateProxyCert :: (Bi w) => SafeSigner -> PublicKey -> w -> ProxyCert w
safeCreateProxyCert ss (PublicKey delegatePk) o = coerce $ ProxyCert sig
  where
    Signature sig = safeSign SignProxySK ss $
                      mconcat
                          ["00", CC.unXPub delegatePk, Bi.serialize' o]

-- | Creates proxy secret key
safeCreatePsk :: (Bi w) => SafeSigner -> PublicKey -> w -> ProxySecretKey w
safeCreatePsk ss delegatePk w =
    ProxySecretKey w (safeToPublic ss) delegatePk $ safeCreateProxyCert ss delegatePk w

-- [CSL-1157] `createProxyCert` and `createProxySecretKey` are not safe and
--   left here because of their implementation details
--   in future should be removed completely, now left for compatibility with tests

-- | Proxy certificate creation from secret key of issuer, public key
-- of delegate and the message space ω.
createProxyCert :: (Bi w) => SecretKey -> PublicKey -> w -> ProxyCert w
createProxyCert = safeCreateProxyCert . fakeSigner

-- | Creates proxy secret key
createPsk :: (Bi w) => SecretKey -> PublicKey -> w -> ProxySecretKey w
createPsk = safeCreatePsk . fakeSigner
