{-# LANGUAGE CPP #-}
-- | Module for safe (zero-memory) signing

module Pos.Crypto.Signing.Types.Safe
       ( EncryptedSecretKey (..)
       , PassPhrase (..)
       , SafeSigner (..)
       , passphraseLength
       , emptyPassphrase
       , mkEncSecretWithSaltUnsafe
       , mkEncSecretUnsafe
       , encToSecret
       , encToPublic
       , noPassEncrypt
       , checkPassMatches
       ) where

import qualified Cardano.Crypto.Wallet as CC
import           Crypto.Random (MonadRandom)
import           Data.ByteArray (ByteArray, ByteArrayAccess, ScrubbedBytes)
import qualified Data.ByteArray as ByteArray
import qualified Data.ByteString as BS
import           Data.Default (Default (..))
import           Data.Semigroup (Semigroup)
import           Formatting (int, sformat, (%))
import           Formatting.Buildable (build)
import qualified Formatting.Buildable as B
import qualified Prelude
import           Universum

import           Pos.Binary.Class (Bi (..), encodeListLen, enforceSize,
                     toCborError)
import qualified Pos.Crypto.Scrypt as S
import           Pos.Crypto.Signing.Types.Signing (PublicKey (..),
                     SecretKey (..), decodeXPrv, encodeXPrv, toPublic)
import           Pos.Util.Log.LogSafe (SecureLog)

-- | Encrypted HD secret key.
data EncryptedSecretKey = EncryptedSecretKey
    { eskPayload :: !CC.XPrv          -- ^ Secret key itself, encrypted with passphrase.
    , eskHash    :: !S.EncryptedPass  -- ^ Hash of passphrase used for key creation.
    }

-- We don't have the @Eq CC.XPrv@ instance here because
-- it is a security issue to compare secret keys. But it
-- can be derived in tests where it doesn't matter.
deriving instance Eq CC.XPrv => Eq EncryptedSecretKey

instance Show EncryptedSecretKey where
    show _ = "<encrypted key>"

instance B.Buildable EncryptedSecretKey where
    build _ = "<encrypted key>"

instance Bi EncryptedSecretKey where
    encode (EncryptedSecretKey sk pph) = encodeListLen 2
                                      <> encodeXPrv sk
                                      <> encode pph
    decode = EncryptedSecretKey
         <$  enforceSize "EncryptedSecretKey" 2
         <*> decodeXPrv
         <*> decode

newtype PassPhrase = PassPhrase ScrubbedBytes
#if MIN_VERSION_base(4,9,0)
    deriving (Eq, Ord, Semigroup, Monoid, NFData, ByteArray, ByteArrayAccess)
#else
    deriving (Eq, Ord, Monoid, NFData, ByteArray, ByteArrayAccess)
#endif

passphraseLength :: Int
passphraseLength = 32

-- | Empty passphrase used in development.
emptyPassphrase :: PassPhrase
emptyPassphrase = PassPhrase mempty

instance Show PassPhrase where
    show _ = "<passphrase>"

instance Buildable PassPhrase where
    build _ = "<passphrase>"

instance Buildable (SecureLog PassPhrase) where
    build _ = "<passphrase>"

instance Default PassPhrase where
    def = emptyPassphrase

instance Bi PassPhrase where
    encode pp = encode (ByteArray.convert pp :: ByteString)
    decode = do
        bs <- decode @ByteString
        let bl = BS.length bs
        -- Currently passphrase may be either 32-byte long or empty (for
        -- unencrypted keys).
        toCborError $ if bl == 0 || bl == passphraseLength
            then Right $ ByteArray.convert bs
            else Left $ sformat
                 ("put@PassPhrase: expected length 0 or "%int%", not "%int)
                 passphraseLength bl

{-instance Monoid PassPhrase where
    mempty = PassPhrase mempty
    mappend (PassPhrase p1) (PassPhrase p2) = PassPhrase (p1 `mappend` p2)-}

-- | SafeSigner datatype to encapsulate sensitive data
data SafeSigner = SafeSigner EncryptedSecretKey PassPhrase
                | FakeSigner SecretKey
                deriving Show

-- | Parameters used to evaluate hash of passphrase.
passScryptParam :: S.ScryptParams
passScryptParam =
    fromMaybe (error "Bad passphrase scrypt parameters") $
    S.mkScryptParams def
        { S.spHashLen = 32  -- maximal passphrase length
        }

-- | Wrap raw secret key, attaching hash to it.
-- Hash is evaluated using given salt.
-- This function assumes that passphrase matches with secret key.
mkEncSecretWithSaltUnsafe
    :: S.Salt -> PassPhrase -> CC.XPrv -> EncryptedSecretKey
mkEncSecretWithSaltUnsafe salt pp payload =
    EncryptedSecretKey payload $ S.encryptPassWithSalt passScryptParam salt pp

-- | Wrap raw secret key, attachind hash to it.
-- Hash is evaluated using generated salt.
-- This function assumes that passphrase matches with secret key.
mkEncSecretUnsafe
    :: (MonadRandom m)
    => PassPhrase -> CC.XPrv -> m EncryptedSecretKey
mkEncSecretUnsafe pp payload =
    EncryptedSecretKey payload <$> S.encryptPass passScryptParam pp

-- | Generate a secret key from encrypted secret key.
encToSecret :: EncryptedSecretKey -> SecretKey
encToSecret (EncryptedSecretKey sk _) = SecretKey sk

-- | Generate a public key using an encrypted secret key and passphrase
encToPublic :: EncryptedSecretKey -> PublicKey
encToPublic = toPublic . encToSecret

-- | Re-wrap unencrypted secret key as an encrypted one.
-- NB: for testing purposes only
noPassEncrypt
    :: SecretKey -> EncryptedSecretKey
noPassEncrypt (SecretKey k) =
    mkEncSecretWithSaltUnsafe S.emptySalt emptyPassphrase k

-- Here with types to avoid module import cycles:
checkPassMatches
    :: (Alternative f)
    => PassPhrase -> EncryptedSecretKey -> f ()
checkPassMatches pp (EncryptedSecretKey _ pph) =
    guard (S.verifyPass passScryptParam pp pph)
