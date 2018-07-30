module Pos.Crypto.Signing.Redeem
       ( redeemKeyGen
       , redeemDeterministicKeyGen
       , redeemSign
       , redeemCheckSig
       , module Pos.Crypto.Signing.Types.Redeem
       ) where

import           Universum

import           Crypto.Error (CryptoFailable (..))
import qualified Crypto.PubKey.Ed25519 as Ed25519
import           Crypto.Random (MonadRandom)
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import           Data.Coerce (coerce)
import qualified Data.Text as T
import           Pos.Binary.Class (Bi, Raw)
import qualified Pos.Binary.Class as Bi
import           Pos.Crypto.Configuration (ProtocolMagic)
import           Pos.Crypto.Signing.Tag (SignTag, signTag)
import           Pos.Crypto.Signing.Types.Redeem

----------------------------------------------------------------------------
-- Conversion and keygens
----------------------------------------------------------------------------

-- | Generate a key pair. It's recommended to run it with 'runSecureRandom'
-- from "Pos.Crypto.Random" because the OpenSSL generator is probably safer
-- than the default IO generator.
redeemKeyGen :: MonadRandom m => m (RedeemPublicKey, RedeemSecretKey)
redeemKeyGen =
  do
    sk <- Ed25519.generateSecretKey
    return (RedeemPublicKey $ Ed25519.toPublic sk, RedeemSecretKey sk)

-- | Create key pair deterministically from 32 bytes.
redeemDeterministicKeyGen
    :: BS.ByteString
    -> Maybe (RedeemPublicKey, RedeemSecretKey)
redeemDeterministicKeyGen seed =
  case Ed25519.secretKey $ (BA.convert seed :: BA.Bytes) of
        CryptoPassed r -> Just (RedeemPublicKey $ Ed25519.toPublic r, RedeemSecretKey r)
        CryptoFailed e -> error $ mappend "Pos.Crypto.Signing.Redeem.hs redeemDeterministicKeyGen failed because " (T.pack $ show e)

----------------------------------------------------------------------------
-- Redeem signatures
----------------------------------------------------------------------------

-- | Encode something with 'Binary' and sign it.
redeemSign ::
       (Bi a)
    => ProtocolMagic
    -> SignTag
    -> RedeemSecretKey
    -> a
    -> RedeemSignature a
redeemSign pm tag k = coerce . redeemSignRaw pm (Just tag) k . Bi.serialize'

-- | Alias for constructor.
redeemSignRaw
    :: ProtocolMagic
    -> Maybe SignTag
    -> RedeemSecretKey
    -> ByteString
    -> RedeemSignature Raw
redeemSignRaw pm mbTag (RedeemSecretKey k) x =
    RedeemSignature (Ed25519.sign k (Ed25519.toPublic k) (BA.convert $ tag <> x :: BA.Bytes) )
  where
    tag = maybe mempty (signTag pm) mbTag

-- CHECK: @redeemCheckSig
-- | Verify a signature.
redeemCheckSig
    :: (Bi a)
    => ProtocolMagic -> SignTag -> RedeemPublicKey -> a -> RedeemSignature a -> Bool
redeemCheckSig pm tag k x s =
    redeemVerifyRaw pm (Just tag) k (Bi.serialize' x) (coerce s)

-- CHECK: @redeemVerifyRaw
-- | Verify raw 'ByteString'.
redeemVerifyRaw
    :: ProtocolMagic
    -> Maybe SignTag
    -> RedeemPublicKey
    -> ByteString
    -> RedeemSignature Raw
    -> Bool
redeemVerifyRaw pm mbTag (RedeemPublicKey k) x (RedeemSignature s) =
    Ed25519.verify k (BA.convert $ tag <> x :: BA.Bytes) s
  where
    tag = maybe mempty (signTag pm) mbTag
