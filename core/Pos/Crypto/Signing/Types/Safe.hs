-- | Module for safe (zero-memory) signing

module Pos.Crypto.Signing.Types.Safe
       ( EncryptedSecretKey (..)
       , PassPhrase (..)
       , SafeSigner (..)
       , passphraseLength
       , emptyPassphrase
       , encToPublic
       ) where

import qualified Cardano.Crypto.Wallet            as CC
import           Data.ByteArray                   (ByteArray, ByteArrayAccess,
                                                   ScrubbedBytes)
import           Data.Default                     (Default (..))
import           Data.Text.Buildable              (build)
import qualified Data.Text.Buildable              as B
import qualified Prelude
import           Universum

import           Pos.Binary.Class                 (Bi)
import           Pos.Crypto.Hashing               (Hash, hash)
import           Pos.Crypto.Signing.Types.Signing (PublicKey (..), SecretKey (..))

data EncryptedSecretKey = EncryptedSecretKey
    { eskPayload :: !CC.XPrv
    , eskHash    :: !(Hash PassPhrase)
    }

instance Show EncryptedSecretKey where
    show _ = "<encrypted key>"

instance B.Buildable EncryptedSecretKey where
    build _ = "<encrypted key>"

-- | Direct comparison of secret keys is a security issue (cc @vincent)
instance Bi EncryptedSecretKey => Eq EncryptedSecretKey where
    a == b = hash a == hash b

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

-- | Generate a public key using an encrypted secret key and passphrase
encToPublic :: EncryptedSecretKey -> PublicKey
encToPublic (EncryptedSecretKey sk _) = PublicKey (CC.toXPub sk)
