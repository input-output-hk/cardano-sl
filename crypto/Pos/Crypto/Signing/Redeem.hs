module Pos.Crypto.Signing.Redeem
       ( redeemKeyGen
       , redeemDeterministicKeyGen
       , redeemSign
       , redeemCheckSig
       , module Pos.Crypto.Signing.Types.Redeem
       ) where

import           Universum

import           Crypto.Random (MonadRandom, getRandomBytes)
import qualified Data.ByteString as BS
import           Data.Coerce (coerce)

import qualified Crypto.Sign.Ed25519 as Ed25519
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
    RedeemSignature (Ed25519.dsign k (tag <> x))
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
    Ed25519.dverify k (tag <> x) s
  where
    tag = maybe mempty (signTag pm) mbTag
