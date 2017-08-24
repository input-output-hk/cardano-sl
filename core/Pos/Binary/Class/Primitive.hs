
-- | Useful functions for serialization/deserialization.

module Pos.Binary.Class.Primitive
       ( serialize
       , serialize'
       -- * Deserialize inside the Decoder monad
       , deserialize
       , deserialize'
       -- * Unsafe deserialization
       , unsafeDeserialize
       , unsafeDeserialize'
       , putCopyBi
       , getCopyBi
       , Raw(..)
       -- * Binary serialization
       , AsBinary (..)
       , AsBinaryClass (..)
       , fromBinaryM
       -- * Temporary functions
       , biSize
       -- * Backward-compatible functions
       , decodeFull
       -- * Low-level, fine-grained functions
       , deserializeOrFail
       , deserializeOrFail'
       ) where

import qualified Codec.CBOR.Decoding           as D
import qualified Codec.CBOR.Read               as CBOR.Read
import qualified Codec.CBOR.Write              as CBOR.Write
import           Control.Exception             (throw)
import           Control.Monad.ST              (ST, runST)
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as BSL
import qualified Data.ByteString.Lazy.Internal as BSL
import           Data.SafeCopy                 (Contained, SafeCopy (..), contain,
                                                safeGet, safePut)
import qualified Data.Serialize                as Cereal (Get, Put)

import           Pos.Binary.Class.Core         (Bi (..))
import           Serokell.Data.Memory.Units    (Byte)
import           Universum

-- | Serialize a Haskell value to an external binary representation.
--
-- The output is represented as a lazy 'BSL.ByteString' and is constructed
-- incrementally.
serialize :: Bi a => a -> BSL.ByteString
serialize = CBOR.Write.toLazyByteString . encode

-- | Serialize a Haskell value to an external binary representation.
--
-- The output is represented as a strict 'BS.ByteString'.
serialize' :: Bi a => a -> BS.ByteString
serialize' = BSL.toStrict . serialize

-- | Deserialize a Haskell value from the external binary representation
-- (which must have been made using 'serialize' or related function).
--
-- /Throws/: @'CBOR.Read.DeserialiseFailure'@ if the given external
-- representation is invalid or does not correspond to a value of the
-- expected type.
unsafeDeserialize :: Bi a => BSL.ByteString -> a
unsafeDeserialize = either throw identity . bimap fst fst . deserializeOrFail

-- | Strict variant of 'deserialize'.
unsafeDeserialize' :: Bi a => BS.ByteString -> a
unsafeDeserialize' = unsafeDeserialize . BSL.fromStrict

-- | Run `decodeFull` in the `Decoder` monad, failing (using Decoder.fail) in
-- case the process failed or not the whole input was consumed.
-- We are not generalising this function to any monad as doing so would allow
-- us to cheat, as not all the monads have a sensible `fail` implementation.
-- Expect the whole input to be consumed.
deserialize :: Bi a => BSL.ByteString -> D.Decoder s a
deserialize = either (fail . toString) return . decodeFull . BSL.toStrict

-- | Strict version of `deserialize`.
deserialize' :: Bi a => BS.ByteString -> D.Decoder s a
deserialize' = deserialize . BSL.fromStrict

-- | Deserialize a Haskell value from the external binary representation,
-- failing if there are leftovers. In a nutshell, the `full` here implies
-- the contract of this function is that what you feed as input needs to
-- be consumed entirely.
decodeFull :: forall a. Bi a => BS.ByteString -> Either Text a
decodeFull bs0 = case deserializeOrFail' bs0 of
  Right (x, leftover) -> case BS.null leftover of
      True  -> pure x
      False ->
          let msg = "decodeFull failed for " <> label (Proxy @a) <> "! Leftover found: " <> show leftover
          in Left $ fromString msg
  Left  (e, _) -> Left $ fromString $ "decodeFull failed for " <> label (Proxy @a) <> ": " <> show e

-- | Deserialize a Haskell value from the external binary representation,
-- returning either (leftover, value) or a (leftover, @'DeserialiseFailure'@).
deserializeOrFail
    :: Bi a
    => BSL.ByteString
    -> Either (CBOR.Read.DeserialiseFailure, BS.ByteString)
              (a, BS.ByteString)
deserializeOrFail bs0 =
    runST (supplyAllInput bs0 =<< deserializeIncremental)
  where
    supplyAllInput _bs (CBOR.Read.Done bs _ x) = return (Right (x, bs))
    supplyAllInput  bs (CBOR.Read.Partial k)  =
      case bs of
        BSL.Chunk chunk bs' -> k (Just chunk) >>= supplyAllInput bs'
        BSL.Empty           -> k Nothing      >>= supplyAllInput BSL.Empty
    supplyAllInput _ (CBOR.Read.Fail bs _ exn) = return (Left (exn, bs))

-- | Strict variant of 'deserializeOrFail'.
deserializeOrFail'
    :: Bi a
    => BS.ByteString
    -> Either (CBOR.Read.DeserialiseFailure, BS.ByteString)
              (a, BS.ByteString)
deserializeOrFail' = deserializeOrFail . BSL.fromStrict

----------------------------------------------------------------------------
-- SafeCopy
----------------------------------------------------------------------------

putCopyBi :: Bi a => a -> Contained Cereal.Put
putCopyBi = contain . safePut . serialize

getCopyBi :: forall a. Bi a => Contained (Cereal.Get a)
getCopyBi = contain $ do
    bs <- safeGet
    case deserializeOrFail bs of
        Left (err, _) -> fail $ "getCopy@" ++ (label (Proxy @a)) <> ": " <> show err
        Right (x, _)  -> return x

----------------------------------------


-- | Deserialize a Haskell value from the external binary representation.
--
-- This allows /input/ data to be provided incrementally, rather than all in one
-- go. It also gives an explicit representation of deserialisation errors.
--
-- Note that the incremental behaviour is only for the input data, not the
-- output value: the final deserialized value is constructed and returned as a
-- whole, not incrementally.
deserializeIncremental :: Bi a => ST s (CBOR.Read.IDecode s a)
deserializeIncremental = CBOR.Read.deserialiseIncremental decode

----------------------------------------------------------------------------
-- Raw
----------------------------------------------------------------------------

-- | A wrapper over 'ByteString' for signalling that a bytestring should be
-- processed as a sequence of bytes, not as a separate entity. It's used in
-- crypto and binary code.
newtype Raw = Raw ByteString
    deriving (Bi, Eq, Ord, Show, Typeable, NFData)

----------------------------------------------------------------------------
-- Helper functions, types, classes
----------------------------------------------------------------------------

-- | A wrapper over 'ByteString' representing a serialized value of
-- type 'a'. This wrapper is used to delay decoding of some data. Note
-- that by default nothing guarantees that the stored 'ByteString' is
-- a valid representation of some value of type 'a'.
newtype AsBinary a = AsBinary
    { getAsBinary :: ByteString
    } deriving (Show, Eq, Ord, Hashable, NFData)

instance SafeCopy (AsBinary a) where
    getCopy = contain $ AsBinary <$> safeGet
    putCopy = contain . safePut . getAsBinary

-- | A simple helper class simplifying work with 'AsBinary'.
class AsBinaryClass a where
    asBinary :: a -> AsBinary a
    fromBinary :: AsBinary a -> Either Text a

-- | Version of 'fromBinary' which works in any 'MonadFail'.
fromBinaryM :: (AsBinaryClass a, MonadFail m) => AsBinary a -> m a
fromBinaryM = either (fail . toString) return . fromBinary

-- | Compute size of something serializable in bytes.
biSize :: Bi a => a -> Byte
biSize = fromIntegral . BS.length . serialize'
{-# INLINE biSize #-}
