{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Pos.Crypto.RedeemSigning
       ( RedeemSecretKey (..)
       , RedeemPublicKey (..)
       , redeemKeyGen
       , redeemDeterministicKeyGen
       , redeemToPublic
       , RedeemSignature (..)
       , redeemSign
       , redeemCheckSig
       ) where

import qualified Crypto.Sign.Ed25519    as Ed25519
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as BSL
import           Data.Coerce            (coerce)
import           Data.Hashable          (Hashable)
import           Data.SafeCopy          (SafeCopy (..), base, contain,
                                         deriveSafeCopySimple, safeGet, safePut)
import qualified Data.Text.Buildable    as B
import           Data.Text.Lazy.Builder (Builder)
import           Formatting             (Format, bprint, build, later, (%))
import qualified Serokell.Util.Base16   as B16
import qualified Serokell.Util.Base64   as Base64 (decode, encode)
import           Serokell.Util.Text     (pairF)
import           Universum

import           Pos.Binary.Class       (Bi, Raw)
import qualified Pos.Binary.Class       as Bi
import           Pos.Crypto.Hashing     (hash, shortHashF)
import           Pos.Crypto.Random      (secureRandomBS)

instance Hashable Ed25519.PublicKey
instance Hashable Ed25519.SecretKey
instance Hashable Ed25519.Signature

instance NFData Ed25519.PublicKey
instance NFData Ed25519.SecretKey
instance NFData Ed25519.Signature

deriveSafeCopySimple 0 'base ''Ed25519.PublicKey
deriveSafeCopySimple 0 'base ''Ed25519.SecretKey
deriveSafeCopySimple 0 'base ''Ed25519.Signature

-- | Wrapper around 'Ed25519.PublicKey'.
newtype RedeemPublicKey = RedeemPublicKey Ed25519.PublicKey
    deriving (Eq, Ord, Show, Generic, NFData, Hashable, Typeable)

-- | Wrapper around 'Ed25519.SecretKey'.
newtype RedeemSecretKey = RedeemSecretKey Ed25519.SecretKey
    deriving (Eq, Ord, Show, Generic, NFData, Hashable)

deriveSafeCopySimple 0 'base ''RedeemPublicKey
deriveSafeCopySimple 0 'base ''RedeemSecretKey

redeemToPublic :: RedeemSecretKey -> RedeemPublicKey
redeemToPublic (RedeemSecretKey k) = RedeemPublicKey (Ed25519.secretToPublicKey k)

-- | Generate a key pair.
redeemKeyGen :: MonadIO m => m (RedeemPublicKey, RedeemSecretKey)
redeemKeyGen = liftIO $ secureRandomBS 32 >>=
    maybe err pure . redeemDeterministicKeyGen
  where err = panic "Pos.Crypto.RedeemSigning.redeemKeyGen:\
                    \ createKeypairFromSeed_ failed"

-- | Create key pair deterministically from 32 bytes.
redeemDeterministicKeyGen
    :: BS.ByteString
    -> Maybe (RedeemPublicKey, RedeemSecretKey)
redeemDeterministicKeyGen seed =
    bimap RedeemPublicKey RedeemSecretKey <$> Ed25519.createKeypairFromSeed_ seed

-- | Wrapper around 'Ed25519.Signature'.
newtype RedeemSignature a = RedeemSignature Ed25519.Signature
    deriving (Eq, Ord, Show, Generic, NFData, Hashable, Typeable)

instance SafeCopy (RedeemSignature a) where
    putCopy (RedeemSignature sig) = contain $ safePut sig
    getCopy = contain $ RedeemSignature <$> safeGet

instance B.Buildable (RedeemSignature a) where
    build _ = "<signature>"

-- | Encode something with 'Binary' and sign it.
redeemSign :: Bi a => RedeemSecretKey -> a -> RedeemSignature a
redeemSign k = coerce . redeemSignRaw k . BSL.toStrict . Bi.encode

-- | Alias for constructor.
redeemSignRaw :: RedeemSecretKey -> ByteString -> RedeemSignature Raw
redeemSignRaw (RedeemSecretKey k) x = RedeemSignature (Ed25519.dsign k x)

-- CHECK: @redeemCheckSig
-- | Verify a signature.
redeemCheckSig :: Bi a => RedeemPublicKey -> a -> RedeemSignature a -> Bool
redeemCheckSig k x s = redeemVerifyRaw k (BSL.toStrict (Bi.encode x)) (coerce s)

-- CHECK: @redeemVerifyRaw
-- | Verify raw 'ByteString'.
redeemVerifyRaw :: RedeemPublicKey -> ByteString -> RedeemSignature Raw -> Bool
redeemVerifyRaw (RedeemPublicKey k) x (RedeemSignature s) = Ed25519.dverify k x s
