-- | Signing done with public/private keys.

module Pos.Crypto.Signing.Types.Signing
       (
       -- * Keys
         PublicKey (..)
       , SecretKey (..)

       , toPublic
       , encodeXPrv
       , decodeXPrv

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
       , fullSignatureHexF
       , parseFullSignature

       , Signed (..)

       -- * Proxy signature scheme
       , ProxyCert (..)
       , fullProxyCertHexF
       , parseFullProxyCert

       , ProxySecretKey (..)
       , ProxySignature (..)

       , isSelfSignedPsk
       ) where

import           Universum hiding (show)

import qualified Cardano.Crypto.Wallet as CC
import qualified Codec.CBOR.Decoding as D
import qualified Codec.CBOR.Encoding as E
import           Control.Lens (_Left)
import           Data.Aeson (FromJSON (..), ToJSON (..))
import           Data.Aeson.TH (defaultOptions, deriveJSON)
import           Data.ByteString.Base58 (bitcoinAlphabet, decodeBase58, encodeBase58)
import           Data.Hashable (Hashable)
import           Data.SafeCopy (SafeCopy (..), base, contain,
                     deriveSafeCopySimple, safeGet, safePut)
import           Data.Text.Lazy.Builder (Builder)
import           Formatting (Format, bprint, build, fitLeft, formatToString,
                     later, sformat, stext, (%), (%.))
import qualified Formatting.Buildable as B
import           Data.Maybe (fromJust)
import           Prelude (show)
import qualified Serokell.Util.Base16 as B16
import qualified Serokell.Util.Base64 as Base64 (decode, formatBase64)
import           Text.JSON.Canonical (JSValue (..), ReportSchemaErrors)
import qualified Text.JSON.Canonical as TJC (FromJSON (..), ToJSON (..))

import           Pos.Binary.Class (Bi (..), encodeListLen, enforceSize)
import qualified Pos.Binary.Class as Bi
import           Pos.Crypto.Hashing (hash)
import           Pos.Crypto.Orphans ()
import           Pos.Util.Json.Parse (tryParseString)
import           Pos.Util.Util (cerealError, toAesonError, toCborError)

----------------------------------------------------------------------------
-- Utilities for From/ToJSON instances
----------------------------------------------------------------------------

fmsg :: ToText s => Text -> Either s a -> Either Text a
fmsg msg = over _Left $ \e ->
    ("Unable to parse json " <> msg <> " reason: ") <> toText e

----------------------------------------------------------------------------
-- Keys, key generation & printing & decoding
----------------------------------------------------------------------------

-- | Wrapper around 'CC.XPub'.
newtype PublicKey = PublicKey CC.XPub
    deriving (Eq, Ord, Show, Generic, NFData, Hashable, Typeable)

instance ToJSON PublicKey where
    toJSON = toJSON . sformat fullPublicKeyF

instance FromJSON PublicKey where
    parseJSON v = parseJSON v >>= toAesonError . fmsg "PublicKey" . parseFullPublicKey

instance Monad m => TJC.ToJSON m PublicKey where
    toJSON = pure . JSString . formatToString fullPublicKeyF

instance ReportSchemaErrors m => TJC.FromJSON m PublicKey where
    fromJSON = tryParseString parseFullPublicKey

encodeXPub :: CC.XPub -> E.Encoding
encodeXPub a = encode $ CC.unXPub a

decodeXPub :: D.Decoder s CC.XPub
decodeXPub = toCborError . over _Left fromString . CC.xpub =<< decode

instance Bi PublicKey where
    encode (PublicKey a) = encodeXPub a
    decode = fmap PublicKey decodeXPub
    encodedSizeExpr _ _ = 66

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

instance B.Buildable PublicKey where
    build = bprint ("pub:"%shortPublicKeyHexF)

instance B.Buildable SecretKey where
    build = bprint ("sec:"%shortPublicKeyHexF) . toPublic

encodeXPrv :: CC.XPrv -> E.Encoding
encodeXPrv a = encode $ CC.unXPrv a

decodeXPrv :: D.Decoder s CC.XPrv
decodeXPrv = toCborError . over _Left fromString . CC.xprv =<< decode @ByteString

instance Bi SecretKey where
    encode (SecretKey a) = encodeXPrv a
    decode = fmap SecretKey decodeXPrv

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

instance FromJSON (Signature w) where
    parseJSON v = parseJSON v >>= toAesonError . fmsg "Signature" . parseFullSignature

instance ToJSON (Signature w) where
    toJSON = toJSON . sformat fullSignatureHexF

instance Monad m => TJC.ToJSON m (Signature w) where
    toJSON = pure . JSString . formatToString fullSignatureHexF

instance (Typeable x, ReportSchemaErrors m) => TJC.FromJSON m (Signature x) where
    fromJSON = tryParseString parseFullSignature

instance SafeCopy (Signature a) where
    putCopy (Signature sig) = contain $ safePut sig
    getCopy = contain $ Signature <$> safeGet

-- | Formatter for 'Signature' to show it in hex.
fullSignatureHexF :: Format r (Signature a -> r)
fullSignatureHexF = later $ \(Signature x) ->
    B16.formatBase16 . CC.unXSignature $ x

-- | Parse 'Signature' from base16 encoded string.
parseFullSignature :: Text -> Either Text (Signature a)
parseFullSignature s = do
    b <- B16.decode s
    Signature <$> first fromString (CC.xsignature b)

encodeXSignature :: CC.XSignature -> E.Encoding
encodeXSignature a = encode $ CC.unXSignature a

decodeXSignature :: D.Decoder s CC.XSignature
decodeXSignature = toCborError . over _Left fromString . CC.xsignature =<< decode

instance Typeable a => Bi (Signature a) where
    encode (Signature a) = encodeXSignature a
    decode = fmap Signature decodeXSignature

-- | Value and signature for this value.
data Signed a = Signed
    { signedValue :: !a              -- ^ Value to be signed
    , signedSig   :: !(Signature a)  -- ^ 'Signature' of 'signedValue'
    } deriving (Show, Eq, Ord, Generic)

instance Bi a => Bi (Signed a) where
    encode (Signed v s) = encodeListLen 2
                       <> encode v
                       <> encode s
    decode = Signed
         <$  enforceSize "Signed" 2
         <*> decode
         <*> decode

instance Bi a => SafeCopy (Signed a) where
    putCopy (Signed v s) = contain $ safePut (Bi.serialize' (v,s))
    getCopy = contain $ do
        bs <- safeGet
        case Bi.decodeFull bs of
            Left err    -> cerealError $ "getCopy@SafeCopy: " <> err
            Right (v,s) -> pure $ Signed v s

----------------------------------------------------------------------------
-- Proxy signing
----------------------------------------------------------------------------

-- | Proxy certificate, made of ω + public key of delegate.
newtype ProxyCert w = ProxyCert { unProxyCert :: CC.XSignature }
    deriving (Eq, Ord, Show, Generic, NFData, Hashable)

instance B.Buildable (ProxyCert w) where
    build _ = "<proxy_cert>"

instance ToJSON (ProxyCert w) where
    toJSON = toJSON . sformat fullProxyCertHexF

instance FromJSON (ProxyCert w) where
    parseJSON v = parseJSON v >>= toAesonError . fmsg "Signature" . parseFullProxyCert

instance Monad m => TJC.ToJSON m (ProxyCert w) where
    toJSON = pure . JSString . formatToString fullProxyCertHexF

instance (Typeable w, ReportSchemaErrors m) => TJC.FromJSON m (ProxyCert w) where
    fromJSON = tryParseString parseFullProxyCert

instance Typeable w => Bi (ProxyCert w) where
    encode (ProxyCert a) = encodeXSignature a
    decode = fmap ProxyCert decodeXSignature

instance SafeCopy (ProxyCert w) where
    putCopy (ProxyCert sig) = contain $ safePut sig
    getCopy = contain $ ProxyCert <$> safeGet

-- | Formatter for 'ProxyCert' to show it in hex.
fullProxyCertHexF :: Format r (ProxyCert a -> r)
fullProxyCertHexF = later $ \(ProxyCert x) ->
    B16.formatBase16 . CC.unXSignature $ x

-- | Parse 'ProxyCert' from base16 encoded string.
parseFullProxyCert :: Text -> Either Text (ProxyCert a)
parseFullProxyCert s = do
    b <- B16.decode s
    ProxyCert <$> first fromString (CC.xsignature b)

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

instance (B.Buildable w) => B.Buildable (ProxySecretKey w) where
    build (UnsafeProxySecretKey w iPk dPk _) =
        bprint ("ProxySk { w = "%build%", iPk = "%build%", dPk = "%build%" }") w iPk dPk

instance Bi w => Bi (ProxySecretKey w) where
    encode UnsafeProxySecretKey{..} =
        encodeListLen 4
        <> encode pskOmega
        <> encode pskIssuerPk
        <> encode pskDelegatePk
        <> encode pskCert
    decode = do
        enforceSize "ProxySecretKey" 4
        pskOmega      <- decode
        pskIssuerPk   <- decode
        pskDelegatePk <- decode
        pskCert       <- decode
        pure UnsafeProxySecretKey {..}

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

instance (B.Buildable w) => B.Buildable (ProxySignature w a) where
    build ProxySignature{..} = bprint ("Proxy signature { psk = "%build%" }") psigPsk

instance (Typeable a, Bi w) =>
         Bi (ProxySignature w a) where
    encode ProxySignature{..} = encodeListLen 2
                             <> encode psigPsk
                             <> encodeXSignature psigSig
    decode = ProxySignature
          <$  enforceSize "ProxySignature" 2
          <*> decode
          <*> decodeXSignature

instance SafeCopy w => SafeCopy (ProxySignature w a) where
    putCopy ProxySignature{..} = contain $ do
        safePut psigPsk
        safePut psigSig
    getCopy = contain $ ProxySignature <$> safeGet <*> safeGet

-- | Checks if delegate and issuer fields of proxy secret key are
-- equal.
isSelfSignedPsk :: ProxySecretKey w -> Bool
isSelfSignedPsk psk = pskIssuerPk psk == pskDelegatePk psk

-- These are *not* orphan instances, these types are defined in this file.
-- However these need to be defined here to avoid TemplateHaskell compile
-- phase errors.

deriveJSON defaultOptions ''ProxySecretKey
deriveSafeCopySimple 0 'base ''PublicKey
deriveSafeCopySimple 0 'base ''SecretKey
deriveSafeCopySimple 0 'base ''ProxySecretKey
