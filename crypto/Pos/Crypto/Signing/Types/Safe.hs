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
import           Data.Default (Default (..))
import           Data.Text.Buildable (build)
import qualified Data.Text.Buildable as B
import qualified Prelude
import           Universum

import           Pos.Binary.Class (Bi)
import qualified Pos.Crypto.Scrypt as S
import           Pos.Crypto.Signing.Types.Signing (PublicKey (..), SecretKey (..), toPublic)

-- | Encrypted HD secret key.
data EncryptedSecretKey = EncryptedSecretKey
    { eskPayload :: !CC.XPrv          -- ^ Secret key itself, encrypted with passphrase.
    , eskHash    :: !S.EncryptedPass  -- ^ Hash of passphrase used for key creation.
    }

instance Show EncryptedSecretKey where
    show _ = "<encrypted key>"

instance B.Buildable EncryptedSecretKey where
    build _ = "<encrypted key>"

newtype PassPhrase = PassPhrase ScrubbedBytes
    deriving (Eq, Ord, Monoid, NFData, ByteArray, ByteArrayAccess)

passphraseLength :: Int
passphraseLength = 32

-- | Empty passphrase used in development.
emptyPassphrase :: PassPhrase
emptyPassphrase = PassPhrase mempty

instance Show PassPhrase where
    show _ = "<passphrase>"

instance Buildable PassPhrase where
    build _ = "<passphrase>"

instance Default PassPhrase where
    def = emptyPassphrase

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
    :: Bi PassPhrase
    => S.Salt -> PassPhrase -> CC.XPrv -> EncryptedSecretKey
mkEncSecretWithSaltUnsafe salt pp payload =
    EncryptedSecretKey payload $ S.encryptPassWithSalt passScryptParam salt pp

-- | Wrap raw secret key, attachind hash to it.
-- Hash is evaluated using generated salt.
-- This function assumes that passphrase matches with secret key.
mkEncSecretUnsafe
    :: (Bi PassPhrase, MonadRandom m)
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
    :: Bi PassPhrase
    => SecretKey -> EncryptedSecretKey
noPassEncrypt (SecretKey k) =
    mkEncSecretWithSaltUnsafe S.emptySalt emptyPassphrase k

-- Here with types to avoid module import cycles:
checkPassMatches
    :: (Bi PassPhrase, Alternative f)
    => PassPhrase -> EncryptedSecretKey -> f ()
checkPassMatches pp (EncryptedSecretKey _ pph) =
    guard (S.verifyPass passScryptParam pp pph)
