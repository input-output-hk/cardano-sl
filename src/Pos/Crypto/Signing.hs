{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

-- | Signing done with public/private keys.
module Pos.Crypto.Signing
       (
       -- * Keys
         PublicKey
       , SecretKey
       , keyGen
       , toPublic
       , formatFullPublicKey
       , fullPublicKeyF
       , parseFullPublicKey

       -- * Signing and verification
       , Signature
       , sign
       , verify

       , Signed
       , mkSigned
       , signedValue
       , signedSig

       -- * Versions for raw bytestrings
       , signRaw
       , verifyRaw
       ) where

import qualified Crypto.Sign.Ed25519    as Ed25519
import           Data.Binary            (Binary)
import qualified Data.Binary            as Binary
import qualified Data.ByteString.Lazy   as BSL
import           Data.Coerce            (coerce)
import           Data.Hashable          (Hashable)
import           Data.SafeCopy          (base, deriveSafeCopySimple)
import           Data.SafeCopy          (SafeCopy (..))
import qualified Data.Text.Buildable    as Buildable
import           Data.Text.Lazy.Builder (Builder)
import           Formatting             (Format, bprint, fitLeft, later, (%), (%.))
import           Universum

import qualified Serokell.Util.Base64   as Base64 (decode, encode)

import           Pos.Crypto.Hashing     (hash, hashHexF)
import           Pos.Crypto.Random      (secureRandomBS)
import           Pos.Util               (Raw, getCopyBinary, putCopyBinary)

----------------------------------------------------------------------------
-- Some orphan instances
----------------------------------------------------------------------------

instance Hashable Ed25519.PublicKey
instance Hashable Ed25519.SecretKey
instance Hashable Ed25519.Signature

instance Binary Ed25519.PublicKey
instance Binary Ed25519.SecretKey
instance Binary Ed25519.Signature

instance NFData Ed25519.PublicKey
instance NFData Ed25519.SecretKey
instance NFData Ed25519.Signature

----------------------------------------------------------------------------
-- Keys, key generation & printing & decoding
----------------------------------------------------------------------------

newtype PublicKey = PublicKey Ed25519.PublicKey
    deriving (Eq, Ord, Show, Binary, NFData, Hashable)

newtype SecretKey = SecretKey Ed25519.SecretKey
    deriving (Eq, Ord, Show, Binary, NFData, Hashable)

instance SafeCopy PublicKey where
    putCopy = putCopyBinary
    getCopy = getCopyBinary "PublicKey"

instance SafeCopy SecretKey where
    putCopy = putCopyBinary
    getCopy = getCopyBinary "SecretKey"

-- | Generate a public key from a secret key. Fast (it just drops some bytes
-- off the secret key).
toPublic :: SecretKey -> PublicKey
toPublic (SecretKey k) = PublicKey (Ed25519.toPublicKey k)

instance Buildable.Buildable PublicKey where
    -- Hash the key, take first 8 chars (that's how GPG does fingerprinting,
    -- except that their binary representation of the key is different)
    build = bprint ("pub:" % fitLeft 8 %. hashHexF) . hash

instance Buildable.Buildable SecretKey where
    build = bprint ("sec:" % fitLeft 8 %. hashHexF) . hash . toPublic

formatFullPublicKey :: PublicKey -> Builder
formatFullPublicKey = Buildable.build . Base64.encode . toS . Binary.encode

fullPublicKeyF :: Format r (PublicKey -> r)
fullPublicKeyF = later formatFullPublicKey

parseFullPublicKey :: Text -> Maybe PublicKey
parseFullPublicKey s =
    case Base64.decode s of
        Left _  -> Nothing
        Right b -> case Binary.decodeOrFail (toS b) of
            Left _ -> Nothing
            Right (unconsumed, _, a)
                | BSL.null unconsumed -> Just a
                | otherwise -> Nothing

-- | Generate a key pair.
keyGen :: MonadIO m => m (PublicKey, SecretKey)
keyGen = liftIO $ do
    seed <- secureRandomBS 32
    case Ed25519.createKeypairFromSeed_ seed of
        Nothing -> panic "Pos.Crypto.Signing.keyGen:\
                         \ createKeypairFromSeed_ failed"
        Just (pk, sk) -> return (PublicKey pk, SecretKey sk)

----------------------------------------------------------------------------
-- Signatures
----------------------------------------------------------------------------

newtype Signature a = Signature Ed25519.Signature
    deriving (Eq, Ord, Show, NFData, Binary)

instance SafeCopy (Signature a) where
    putCopy = putCopyBinary
    getCopy = getCopyBinary "Signature"

instance Buildable.Buildable (Signature a) where
    build _ = "<signature>"

-- | Encode something with 'Binary' and sign it.
sign :: (Binary a) => SecretKey -> a -> Signature a
sign k = coerce . signRaw k . toS . Binary.encode

signRaw :: SecretKey -> ByteString -> Signature Raw
signRaw (SecretKey k) x = Signature (Ed25519.dsign k x)

-- | Verify a signature.
verify :: Binary a => PublicKey -> a -> Signature a -> Bool
verify k x s = verifyRaw k (toS (Binary.encode x)) (coerce s)

verifyRaw :: PublicKey -> ByteString -> Signature Raw -> Bool
verifyRaw (PublicKey k) x (Signature s) = Ed25519.dverify k x s

-- | Value and signature for this value.
data Signed a = Signed
    { signedValue :: !a
    , signedSig   :: !(Signature a)
    } deriving (Show, Eq, Ord, Generic)

instance Binary a => Binary (Signed a)

mkSigned :: (Binary a) => SecretKey -> a -> Signed a
mkSigned sk x = Signed x (sign sk x)

deriveSafeCopySimple 0 'base ''Signed
