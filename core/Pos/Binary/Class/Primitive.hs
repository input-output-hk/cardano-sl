-- | Useful functions for serialization/deserialization.

module Pos.Binary.Class.Primitive
       (
       -- * Serialization
         serialize
       , serialize'

       -- * Deserialize inside the Decoder monad
       , deserialize'
       -- * Low-level, fine-grained functions
       , deserializeOrFailRaw
       , deserializeThrow
       -- * Backward-compatible functions
       , decodeFull
       , decodeFullNoCheck

       -- * Safecopy
       , putCopyBi
       , getCopyBi
       -- * Bytestring wrapper
       , Raw(..)
       -- * Binary serialization
       , AsBinary (..)
       , AsBinaryClass (..)
       , fromBinaryM
       -- * Temporary functions
       , biSize

       -- * CBOR in CBOR
       , encodeKnownCborDataItem
       , encodeUnknownCborDataItem
       , decodeKnownCborDataItem
       , decodeUnknownCborDataItem

       -- * Cyclic redundancy check
       , encodeCrcProtected
       , decodeCrcProtected
       ) where

import qualified Codec.CBOR.Decoding           as D
import qualified Codec.CBOR.Encoding           as E
import qualified Codec.CBOR.Read               as CBOR.Read
import qualified Codec.CBOR.Write              as CBOR.Write
import           Control.Exception             (throw)
import           Control.Monad.ST              (ST, runST)
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as BSL
import qualified Data.ByteString.Lazy.Internal as BSL
import           Data.Default                  (Default (..))
import           Data.SafeCopy                 (Contained, SafeCopy (..), contain,
                                                safeGet, safePut)
import qualified Data.Serialize                as Cereal (Get, Put)
import           Data.Typeable                 (typeOf)

import           Data.Digest.CRC32             (CRC32 (..))
import           Formatting                    (formatToString, shown, (%))
import           Pos.Binary.Class.Core         (Bi (..), Decoder, DecoderConfig (..),
                                                enforceSize, toDecoder)
import           Serokell.Data.Memory.Units    (Byte)
import           Universum

----------------------------------------------------------------------------
-- Serialization
----------------------------------------------------------------------------

-- | Serialize a Haskell value to an external binary representation.
--
-- The output is represented as a lazy 'LByteString' and is constructed
-- incrementally.
serialize :: Bi a => a -> LByteString
serialize = CBOR.Write.toLazyByteString . encode

-- | Serialize a Haskell value to an external binary representation.
--
-- The output is represented as a strict 'BS.ByteString'.
serialize' :: Bi a => a -> BS.ByteString
serialize' = BSL.toStrict . serialize

----------------------------------------------------------------------------
-- Deserialization
----------------------------------------------------------------------------

-- | Run `decodeFull` in the `Decoder` monad, failing (using Decoder.fail) in
-- case the process failed or not the whole input was consumed.
-- We are not generalising this function to any monad as doing so would allow
-- us to cheat, as not all the monads have a sensible `fail` implementation.
-- Expect the whole input to be consumed.
deserialize' :: Bi a => BS.ByteString -> Decoder s a
deserialize' = either (fail . toString) return . decodeFull

-- Deserialize a Haskell value from the external binary representation,
-- returning either (leftover, value) or a (leftover, @'DeserialiseFailure'@).
deserializeOrFailRaw
    :: Bi a
    => DecoderConfig
    -> LByteString
    -> Either (CBOR.Read.DeserialiseFailure, BS.ByteString)
              (a, BS.ByteString)
deserializeOrFailRaw dc bs0 =
    runST (supplyAllInput bs0 =<< deserializeIncremental)
  where
    supplyAllInput _bs (CBOR.Read.Done bs _ x) = return (Right (x, bs))
    supplyAllInput  bs (CBOR.Read.Partial k)  =
      case bs of
        BSL.Chunk chunk bs' -> k (Just chunk) >>= supplyAllInput bs'
        BSL.Empty           -> k Nothing      >>= supplyAllInput BSL.Empty
    supplyAllInput _ (CBOR.Read.Fail bs _ exn) = return (Left (exn, bs))

    -- | Deserialize a Haskell value from the external binary representation.
    --
    -- This allows /input/ data to be provided incrementally, rather than all in one
    -- go. It also gives an explicit representation of deserialisation errors.
    --
    -- Note that the incremental behaviour is only for the input data, not the
    -- output value: the final deserialized value is constructed and returned as a
    -- whole, not incrementally.
    deserializeIncremental :: Bi a => ST s (CBOR.Read.IDecode s a)
    deserializeIncremental =
        CBOR.Read.deserialiseIncremental (runReaderT decode dc)

-- | Try to deserialize value or fail.
--
-- /Throws/: @'CBOR.Read.DeserialiseFailure'@ if the given external
-- representation is invalid or does not correspond to a value of the
-- expected type.
deserializeThrow :: (Bi a) => LByteString -> a
deserializeThrow =
    either throw identity . bimap fst fst . deserializeOrFailRaw def

-- This is intermediate function that inspects deserialization result
-- and fails even if we've managed to parse string prefix (there's a
-- nonzero leftover).
decodeFullProcess ::
       forall a. (Bi a)
    => Either (CBOR.Read.DeserialiseFailure, BS.ByteString) (a, BS.ByteString)
    -> Either Text a
decodeFullProcess = \case
    Right (x, leftover) -> case BS.null leftover of
        True  -> pure x
        False ->
            let msg = "decodeFull failed for " <> label (Proxy @a) <>
                      "! Leftover found: " <> show leftover
            in Left $ fromString msg
    Left  (e, _) ->
        Left $ fromString $ "decodeFull failed for " <> label (Proxy @a) <>
                            ": " <> show e

-- | Deserialize a Haskell value from the external binary representation,
-- failing if there are leftovers. In a nutshell, the `full` here implies
-- the contract of this function is that what you feed as input needs to
-- be consumed entirely. NoCheck version corresponds to passing @dcNoCheck@
-- to decoder parser.
decodeFull :: forall a. Bi a => BS.ByteString -> Either Text a
decodeFull =
    decodeFullProcess . deserializeOrFailRaw def . BSL.fromStrict

-- | Same as 'decodeFull', bot doesn't perform extra checks. See 'dcNoCheck'.
decodeFullNoCheck :: forall a. Bi a => BS.ByteString -> Either Text a
decodeFullNoCheck =
    decodeFullProcess .
    deserializeOrFailRaw (def { _dcNoCheck = True }) .
    BSL.fromStrict

----------------------------------------------------------------------------
-- SafeCopy
----------------------------------------------------------------------------

putCopyBi :: Bi a => a -> Contained Cereal.Put
putCopyBi = contain . safePut . serialize

-- Should there be a nocheck version of this function too?
getCopyBi :: forall a. Bi a => Contained (Cereal.Get a)
getCopyBi = contain $ do
    bs <- safeGet
    case deserializeOrFailRaw def bs of
        Left (err, _) -> fail $ "getCopy@" ++ (label (Proxy @a)) <> ": " <> show err
        Right (x, _)  -> return x

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

----------------------------------------------------------------------------
-- CBORDataItem
-- https://tools.ietf.org/html/rfc7049#section-2.4.4.1
----------------------------------------------------------------------------

-- | Encode and serialise the given `a` and sorrounds it with the semantic tag 24.
-- In CBOR diagnostic notation:
-- >>> 24(h'DEADBEEF')
encodeKnownCborDataItem :: Bi a => a -> E.Encoding
encodeKnownCborDataItem = encodeUnknownCborDataItem . serialize'

-- | Like `encodeKnownCborDataItem`, but assumes nothing about the shape of
-- input object, so that it must be passed as a binary `ByteString` blob.
-- It's the caller responsibility to ensure the input `ByteString` correspond
-- indeed to valid, previously-serialised CBOR data.
encodeUnknownCborDataItem :: ByteString -> E.Encoding
encodeUnknownCborDataItem x = E.encodeTag 24 <> encode x

-- | Remove the the semantic tag 24 from the enclosed CBOR data item,
-- failing if the tag cannot be found.
decodeCborDataItemTag :: Decoder s ()
decodeCborDataItemTag = do
    t <- toDecoder D.decodeTagCanonical
    when (t /= 24) $ fail $
        "decodeCborDataItem: expected a bytestring with \
        \CBOR (marked by tag 24), found tag: " <> show t

-- | Remove the the semantic tag 24 from the enclosed CBOR data item,
-- decoding back the inner `ByteString` as a proper Haskell type.
-- Consume its input in full.
decodeKnownCborDataItem :: Bi a => Decoder s a
decodeKnownCborDataItem = do
    bs <- decodeUnknownCborDataItem
    case decodeFull bs of
        Left e  -> fail (toString e)
        Right v -> pure v

-- | Like `decodeKnownCborDataItem`, but assumes nothing about the Haskell
-- type we want to deserialise back, therefore it yields the `ByteString`
-- Tag 24 sorrounded (stripping such tag away).
-- In CBOR notation, if the data was serialised as:
-- >>> 24(h'DEADBEEF')
-- then `decodeUnknownCborDataItem` yields the inner 'DEADBEEF', unchanged.
decodeUnknownCborDataItem :: Decoder s ByteString
decodeUnknownCborDataItem = do
    decodeCborDataItemTag
    toDecoder D.decodeBytes

----------------------------------------------------------------------------
-- CRC
----------------------------------------------------------------------------

-- | Encodes a type `a` , protecting it from tampering/network-transport-alteration by
-- protecting it with a CRC.
encodeCrcProtected :: Bi a => a -> E.Encoding
encodeCrcProtected x = E.encodeListLen 2 <> encodeUnknownCborDataItem body <> encode (crc32 body)
  where
    body = serialize' x

-- | Decodes a CBOR blob into a type `a`, checking the serialised CRC corresponds to the computed one.
decodeCrcProtected :: forall s a. Bi a => Decoder s a
decodeCrcProtected = do
    enforceSize ("decodeCrcProtected: " <> show (typeOf (Proxy @a))) 2
    body <- decodeUnknownCborDataItem
    expectedCrc  <- decode
    let actualCrc :: Word32
        actualCrc = crc32 body
    let crcErrorFmt = "decodeCrcProtected, expected CRC " % shown % " was not the computed one, which was " % shown
    when (actualCrc /= expectedCrc) $ fail (formatToString crcErrorFmt expectedCrc actualCrc)
    case decodeFull body of
      Left e  -> fail (toString e)
      Right x -> pure x
