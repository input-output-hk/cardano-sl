{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Signing done with public/private keys.

module Pos.Crypto.Signing.Types.Signing
       (
       -- * Keys
         PublicKey (..)
       , SecretKey (..)

       , toPublic

       , formatFullPublicKey
       , fullPublicKeyF
       , fullPublicKeyHexF
       , shortPublicKeyHexF
       , parseFullPublicKey
       , decodeBase58PublicKey
       , encodeBase58PublicKey
       , Base58PublicKeyError (..)

       -- * Signing and verification
       , Signature (..)

       , Signed (..)

       -- * Proxy signature scheme
       , ProxyCert (..)
       , ProxySecretKey (..)
       , ProxySignature (..)

       , isSelfSignedPsk
       ) where

import qualified Cardano.Crypto.Wallet as CC
import           Data.Hashable (Hashable)
import qualified Data.Hashable as Hashable
import           Data.Maybe (fromJust)
import qualified Data.Text.Buildable as B
import           Data.Text.Lazy.Builder (Builder)
import           Formatting (Format, bprint, build, fitLeft, later, stext, (%), (%.))
import           Prelude (show)
import qualified Serokell.Util.Base16 as B16
import qualified Serokell.Util.Base64 as Base64 (decode, formatBase64)
import           Universum hiding (show)
import           Data.ByteString.Base58 (bitcoinAlphabet, decodeBase58, encodeBase58)

import           Pos.Binary.Class (Bi)
import           Pos.Crypto.Hashing (hash)

----------------------------------------------------------------------------
-- Orphan instances
----------------------------------------------------------------------------

instance Eq CC.XPub where
    a == b = CC.unXPub a == CC.unXPub b

instance Ord CC.XPub where
    compare = comparing CC.unXPub

instance Show CC.XPub where
    show = show . CC.unXPub

instance Hashable CC.XPub where
    hashWithSalt n = Hashable.hashWithSalt n . CC.unXPub

----------------------------------------------------------------------------
-- Keys, key generation & printing & decoding
----------------------------------------------------------------------------

-- | Wrapper around 'CC.XPub'.
newtype PublicKey = PublicKey CC.XPub
    deriving (Eq, Ord, Show, Generic, NFData, Hashable, Typeable)

-- | Wrapper around 'CC.XPrv'.
newtype SecretKey = SecretKey CC.XPrv
    deriving (NFData)

-- | Generate a public key from a secret key. Fast (it just drops some bytes
-- off the secret key).
toPublic :: SecretKey -> PublicKey
toPublic (SecretKey k) = PublicKey (CC.toXPub k)

-- | Direct comparison of secret keys is a security issue (cc @vincent)
instance Bi SecretKey => Eq SecretKey where
    a == b = hash a == hash b

instance Show SecretKey where
    show sk = "<secret of " ++ show (toPublic sk) ++ ">"

instance Bi PublicKey => B.Buildable PublicKey where
    build = bprint ("pub:"%shortPublicKeyHexF)

instance Bi PublicKey => B.Buildable SecretKey where
    build = bprint ("sec:"%shortPublicKeyHexF) . toPublic

-- | 'Builder' for 'PublicKey' to show it in base64 encoded form.
formatFullPublicKey :: PublicKey -> Builder
formatFullPublicKey (PublicKey pk) =
    Base64.formatBase64 . CC.unXPub $ pk

-- | Formatter for 'PublicKey' to show it in base64.
fullPublicKeyF :: Format r (PublicKey -> r)
fullPublicKeyF = later formatFullPublicKey

-- | Formatter for 'PublicKey' to show it in hex.
fullPublicKeyHexF :: Format r (PublicKey -> r)
fullPublicKeyHexF = later $ \(PublicKey x) -> B16.formatBase16 . CC.unXPub $ x

-- | Formatter for 'PublicKey' to show it in hex, but only first 8 chars.
shortPublicKeyHexF :: Format r (PublicKey -> r)
shortPublicKeyHexF = fitLeft 8 %. fullPublicKeyHexF

-- | Parse 'PublicKey' from base64 encoded string.
parseFullPublicKey :: Text -> Either Text PublicKey
parseFullPublicKey s = do
    b <- Base64.decode s
    PublicKey <$> first fromString (CC.xpub b)

-- | Possible problems with Base58-encoded extended public key.
data Base58PublicKeyError
    = PublicKeyNotBase58Form
    | InvalidPublicKey !Text
    deriving Show

instance Buildable Base58PublicKeyError where
    build PublicKeyNotBase58Form =
        "Extended public key is not in Base58-format."
    build (InvalidPublicKey msg) =
        bprint ("Extended public key is invalid: "%stext) msg

-- | Decode extended public key from Base58-encoded form.
decodeBase58PublicKey :: Text -> Either Base58PublicKeyError PublicKey
decodeBase58PublicKey encodedXPub = do
    let rawExtPubKey = decodeBase58 bitcoinAlphabet . encodeUtf8 $ encodedXPub
    when (isNothing rawExtPubKey) $
        Left PublicKeyNotBase58Form

    let extPubKey = CC.xpub $ fromJust rawExtPubKey
    case extPubKey of
        Left problem -> Left $ InvalidPublicKey (toText problem)
        Right xPub -> Right $ PublicKey xPub

-- | Encode 'PublicKey' in Base58-encoded form. We use it for integration tests.
encodeBase58PublicKey :: PublicKey -> Text
encodeBase58PublicKey (PublicKey xPub) = encodedXPub
  where
    encodedXPub  = decodeUtf8 $ encodeBase58 bitcoinAlphabet rawExtPubKey
    rawExtPubKey = CC.unXPub xPub

----------------------------------------------------------------------------
-- Signatures
----------------------------------------------------------------------------

-- | Wrapper around 'CC.XSignature'.
newtype Signature a = Signature CC.XSignature
    deriving (Eq, Ord, Show, Generic, NFData, Hashable, Typeable)

instance B.Buildable (Signature a) where
    build _ = "<signature>"

-- | Value and signature for this value.
data Signed a = Signed
    { signedValue :: !a              -- ^ Value to be signed
    , signedSig   :: !(Signature a)  -- ^ 'Signature' of 'signedValue'
    } deriving (Show, Eq, Ord, Generic)

----------------------------------------------------------------------------
-- Proxy signing
----------------------------------------------------------------------------

-- | Proxy certificate, made of ω + public key of delegate.
newtype ProxyCert w = ProxyCert { unProxyCert :: CC.XSignature }
    deriving (Eq, Ord, Show, Generic, NFData, Hashable)

instance B.Buildable (ProxyCert w) where
    build _ = "<proxy_cert>"

-- | Convenient wrapper for secret key, that's basically ω plus
-- certificate.
data ProxySecretKey w = UnsafeProxySecretKey
    { pskOmega      :: w
    , pskIssuerPk   :: PublicKey
    , pskDelegatePk :: PublicKey
    , pskCert       :: ProxyCert w
    } deriving (Eq, Ord, Show, Generic)

instance NFData w => NFData (ProxySecretKey w)
instance Hashable w => Hashable (ProxySecretKey w)

instance (B.Buildable w, Bi PublicKey) => B.Buildable (ProxySecretKey w) where
    build (UnsafeProxySecretKey w iPk dPk _) =
        bprint ("ProxySk { w = "%build%", iPk = "%build%", dPk = "%build%" }") w iPk dPk

-- | Delegate signature made with certificate-based permission. @w@
-- stays for message type used in proxy (ω in the implementation
-- notes), @a@ for type of message signed.
--
-- We add whole psk as a field because otherwise we can't verify sig
-- in heavyweight psk transitive delegation: i → x → d, we have psk
-- from x to d, slot leader is i.
data ProxySignature w a = ProxySignature
    { psigPsk :: ProxySecretKey w
    , psigSig :: CC.XSignature
    } deriving (Eq, Ord, Show, Generic)

instance NFData w => NFData (ProxySignature w a)
instance Hashable w => Hashable (ProxySignature w a)

instance (B.Buildable w, Bi PublicKey) => B.Buildable (ProxySignature w a) where
    build ProxySignature{..} = bprint ("Proxy signature { psk = "%build%" }") psigPsk

-- | Checks if delegate and issuer fields of proxy secret key are
-- equal.
isSelfSignedPsk :: ProxySecretKey w -> Bool
isSelfSignedPsk psk = pskIssuerPk psk == pskDelegatePk psk
