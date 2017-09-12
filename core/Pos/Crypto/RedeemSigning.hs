module Pos.Crypto.RedeemSigning
       ( RedeemSecretKey (..)
       , RedeemPublicKey (..)
       , redeemKeyGen
       , redeemDeterministicKeyGen
       , redeemToPublic
       , redeemPkBuild
       , redeemPkB64F
       , redeemPkB64ShortF
       , RedeemSignature (..)
       , redeemSign
       , redeemCheckSig
       ) where

import           Crypto.Random        (MonadRandom, getRandomBytes)
import qualified Data.ByteString      as BS
import           Data.Coerce          (coerce)
import           Data.Hashable        (Hashable)
import qualified Data.Text.Buildable  as B
import           Formatting           (Format, bprint, fitLeft, later, (%), (%.))
import           Serokell.Util.Base64 (formatBase64)
import           Universum

import qualified Crypto.Sign.Ed25519  as Ed25519
import           Pos.Binary.Class     (Bi, Raw)
import qualified Pos.Binary.Class     as Bi
import           Pos.Crypto.SignTag   (SignTag, signTag)


----------------------------------------------------------------------------
-- Underlying wrappers' instances
----------------------------------------------------------------------------

instance Hashable Ed25519.PublicKey
instance Hashable Ed25519.SecretKey
instance Hashable Ed25519.Signature

instance NFData Ed25519.PublicKey
instance NFData Ed25519.SecretKey
instance NFData Ed25519.Signature

----------------------------------------------------------------------------
-- PK/SK and formatters
----------------------------------------------------------------------------

-- | Wrapper around 'Ed25519.PublicKey'.
newtype RedeemPublicKey = RedeemPublicKey Ed25519.PublicKey
    deriving (Eq, Ord, Show, Generic, NFData, Hashable, Typeable)

-- | Wrapper around 'Ed25519.SecretKey'.
newtype RedeemSecretKey = RedeemSecretKey Ed25519.SecretKey
    deriving (Eq, Ord, Show, Generic, NFData, Hashable)

redeemPkB64F :: Format r (RedeemPublicKey -> r)
redeemPkB64F =
    later $ \(RedeemPublicKey pk) -> formatBase64 $ Ed25519.openPublicKey pk

redeemPkB64ShortF :: Format r (RedeemPublicKey -> r)
redeemPkB64ShortF = fitLeft 8 %. redeemPkB64F

instance B.Buildable RedeemPublicKey where
    build = bprint ("redeem_pk:"%redeemPkB64F)

instance B.Buildable RedeemSecretKey where
    build = bprint ("redeem_sec_of_pk:"%redeemPkB64F) . redeemToPublic

----------------------------------------------------------------------------
-- Conversion and keygens
----------------------------------------------------------------------------

-- | Generate a key pair. It's recommended to run it with 'runSecureRandom'
-- from "Pos.Crypto.Random" because the OpenSSL generator is probably safer
-- than the default IO generator.
redeemKeyGen :: MonadRandom m => m (RedeemPublicKey, RedeemSecretKey)
redeemKeyGen =
    getRandomBytes 32 >>=
    maybe err pure . redeemDeterministicKeyGen
  where
    err = error "Pos.Crypto.RedeemSigning.redeemKeyGen: createKeypairFromSeed_ failed"

-- | Create key pair deterministically from 32 bytes.
redeemDeterministicKeyGen
    :: BS.ByteString
    -> Maybe (RedeemPublicKey, RedeemSecretKey)
redeemDeterministicKeyGen seed =
    bimap RedeemPublicKey RedeemSecretKey <$> Ed25519.createKeypairFromSeed_ seed

-- | Public key derivation function
redeemToPublic :: RedeemSecretKey -> RedeemPublicKey
redeemToPublic (RedeemSecretKey k) = RedeemPublicKey (Ed25519.secretToPublicKey k)

-- | Creates a public key from 32 byte bytestring, fails with 'error'
-- otherwise.
redeemPkBuild :: ByteString -> RedeemPublicKey
redeemPkBuild bs
    | BS.length bs /= 32 =
        error $
        "consRedeemPk: failed to form pk, wrong bs length: " <> show (BS.length bs) <>
        ", when should be 32"
    | otherwise = RedeemPublicKey $ Ed25519.PublicKey $ bs

----------------------------------------------------------------------------
-- Redeem signatures
----------------------------------------------------------------------------

-- | Wrapper around 'Ed25519.Signature'.
newtype RedeemSignature a = RedeemSignature Ed25519.Signature
    deriving (Eq, Ord, Show, Generic, NFData, Hashable, Typeable)

instance B.Buildable (RedeemSignature a) where
    build _ = "<redeem signature>"

-- | Encode something with 'Binary' and sign it.
redeemSign :: Bi a => SignTag -> RedeemSecretKey -> a -> RedeemSignature a
redeemSign tag k = coerce . redeemSignRaw (Just tag) k . Bi.serialize'

-- | Alias for constructor.
redeemSignRaw
    :: Maybe SignTag
    -> RedeemSecretKey
    -> ByteString
    -> RedeemSignature Raw
redeemSignRaw mbTag (RedeemSecretKey k) x =
    RedeemSignature (Ed25519.dsign k (tag <> x))
  where
    tag = maybe mempty signTag mbTag

-- CHECK: @redeemCheckSig
-- | Verify a signature.
redeemCheckSig
    :: Bi a
    => SignTag -> RedeemPublicKey -> a -> RedeemSignature a -> Bool
redeemCheckSig tag k x s =
    redeemVerifyRaw (Just tag) k (Bi.serialize' x) (coerce s)

-- CHECK: @redeemVerifyRaw
-- | Verify raw 'ByteString'.
redeemVerifyRaw
    :: Maybe SignTag
    -> RedeemPublicKey
    -> ByteString
    -> RedeemSignature Raw
    -> Bool
redeemVerifyRaw mbTag (RedeemPublicKey k) x (RedeemSignature s) =
    Ed25519.dverify k (tag <> x) s
  where
    tag = maybe mempty signTag mbTag
