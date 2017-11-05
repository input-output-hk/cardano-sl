{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}

-- | Bi typeclass and most basic functions.

module Pos.Binary.Class.Core
    ( Bi (..)
    , DecoderConfig (..)
    , dcNoCheck
    , Decoder
    , toDecoder
    , encodeBinary
    , decodeBinary
    , enforceSize
    , matchSize
    , decodeListLenCanonical
    , decodeListLenCanonicalOf
    -- * CBOR re-exports
    , E.encodeListLen
    , E.Encoding
    , CBOR.Read.deserialiseIncremental
    , CBOR.Write.toLazyByteString
    , CBOR.Read.IDecode(..)
    -- * GHC-Generics-based encoding & decoding
    , genericEncode
    , genericDecode
    ) where

import qualified Codec.CBOR.Decoding        as D
import qualified Codec.CBOR.Encoding        as E
import qualified Codec.CBOR.Read            as CBOR.Read
import qualified Codec.CBOR.Write           as CBOR.Write
import           Control.Lens               (makeLenses)
import           Control.Monad.Reader       (mapReaderT)
import qualified Data.Binary                as Binary
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as BS.Lazy
import           Data.Default               (Default (..))
import           Data.Fixed                 (Fixed (..), Nano)
import qualified Data.HashMap.Strict        as HM
import qualified Data.HashSet               as HS
import qualified Data.Map                   as M
import qualified Data.Set                   as S
import           Data.Tagged                (Tagged (..))
import qualified Data.Text                  as Text
import           Data.Time.Units            (Microsecond, Millisecond)
import           Data.Typeable              (typeRep)
import qualified Data.Vector                as Vector
import qualified Data.Vector.Generic        as Vector.Generic
import qualified GHC.Generics               as G
import           Serokell.Data.Memory.Units (Byte, fromBytes, toBytes)
import           Universum


----------------------------------------------------------------------------
-- Class definition
----------------------------------------------------------------------------

-- | Extra configuration for decoder.
data DecoderConfig = DecoderConfig
    { _dcNoCheck :: Bool
      -- ^ If this flag is set to 'True' decoder should try avoiding
      -- all the datatype integrity checks. It's mostly done to speed
      -- up deserialiation for objects that we serialized securely
      -- before (e.g. something we put into base was in memory once,
      -- so it must be correct).
    } deriving Show

makeLenses ''DecoderConfig

instance Default DecoderConfig where
    def = DecoderConfig { _dcNoCheck = False }

type Decoder s a = ReaderT DecoderConfig (D.Decoder s) a

toDecoder :: D.Decoder s a -> Decoder s a
toDecoder = lift

class Typeable a => Bi a where
    encode :: a -> E.Encoding
    decode :: Decoder s a

    label :: Proxy a -> String
    label = show . typeRep

    encodeList :: [a] -> E.Encoding
    encodeList = defaultEncodeList

    decodeList :: Decoder s [a]
    decodeList = defaultDecodeList

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

encodeBinary :: Binary.Binary a => a -> E.Encoding
encodeBinary = encode . BS.Lazy.toStrict . Binary.encode

decodeBinary :: Binary.Binary a => Decoder s a
decodeBinary = do
    x <- decode @ByteString
    case Binary.decodeOrFail (BS.Lazy.fromStrict x) of
        Left (_, _, err) -> fail err
        Right (bs, _, res)
            | BS.Lazy.null bs -> pure res
            | otherwise       -> fail "decodeBinary: unconsumed input"

-- | Enforces that the input size is the same as the decoded one,
-- failing in case it's not.
enforceSize :: String -> Int -> Decoder s ()
enforceSize lbl requestedSize =
    decodeListLenCanonical >>= matchSize requestedSize lbl

-- | Compare two sizes, failing if they are not equal.
matchSize :: Int -> String -> Int -> Decoder s ()
matchSize requestedSize lbl actualSize =
  when (actualSize /= requestedSize) $
    fail (lbl <> " failed the size check. Expected " <>
          show requestedSize <> ", found " <> show actualSize)

decodeListLenCanonical :: Decoder s Int
decodeListLenCanonical = toDecoder D.decodeListLenCanonical

decodeListLenCanonicalOf :: Int -> Decoder s ()
decodeListLenCanonicalOf = toDecoder . D.decodeListLenCanonicalOf

-- | Default @'E.Encoding'@ for list types.
defaultEncodeList :: Bi a => [a] -> E.Encoding
defaultEncodeList xs = E.encodeListLenIndef
                    <> Universum.foldr (\x r -> encode x <> r) E.encodeBreak xs

-- | Default @'Decoder'@ for list types.
defaultDecodeList :: forall s a . Bi a => Decoder s [a]
defaultDecodeList = do
    toDecoder D.decodeListLenIndef
    let decInf :: D.Decoder s a -> D.Decoder s [a]
        decInf = D.decodeSequenceLenIndef (flip (:)) [] reverse
    mapReaderT decInf decode

----------------------------------------------------------------------------
-- Primitive types
----------------------------------------------------------------------------

instance Bi () where
    encode = const E.encodeNull
    decode = toDecoder D.decodeNull

instance Bi Bool where
    encode = E.encodeBool
    decode = toDecoder D.decodeBool

instance Bi Char where
    encode c = E.encodeString (Text.singleton c)
    decode = do t <- toDecoder D.decodeString
                if Text.length t == 1
                  then return $! Text.head t
                  else fail "expected a single char, found a string"

    -- For [Char]/String we have a special encoding
    encodeList cs = E.encodeString (toText cs)
    decodeList    = do txt <- toDecoder D.decodeString
                       return (toString txt) -- unpack lazily

----------------------------------------------------------------------------
-- Numeric data
----------------------------------------------------------------------------

instance Bi Integer where
    encode = E.encodeInteger
    decode = toDecoder D.decodeIntegerCanonical

instance Bi Word where
    encode = E.encodeWord
    decode = toDecoder D.decodeWordCanonical

instance Bi Word8 where
    encode = E.encodeWord8
    decode = toDecoder D.decodeWord8Canonical

instance Bi Word16 where
    encode = E.encodeWord16
    decode = toDecoder D.decodeWord16Canonical

instance Bi Word32 where
    encode = E.encodeWord32
    decode = toDecoder D.decodeWord32Canonical

instance Bi Word64 where
    encode = E.encodeWord64
    decode = toDecoder D.decodeWord64Canonical

instance Bi Int where
    encode = E.encodeInt
    decode = toDecoder D.decodeIntCanonical

instance Bi Float where
    encode = E.encodeFloat
    decode = toDecoder D.decodeFloatCanonical

instance Bi Int32 where
    encode = E.encodeInt32
    decode = toDecoder D.decodeInt32Canonical

instance Bi Int64 where
    encode = E.encodeInt64
    decode = toDecoder D.decodeInt64Canonical

instance Bi Nano where
    encode (MkFixed resolution) = encode resolution
    decode = MkFixed <$> decode

instance Bi Void where
    decode = fail "instance Bi Void: you shouldn't try to deserialize Void"
    encode = error "instance Bi Void: you shouldn't try to serialize Void"

----------------------------------------------------------------------------
-- Tagged
----------------------------------------------------------------------------

instance (Typeable s, Bi a) => Bi (Tagged s a) where
    encode (Tagged a) = encode a
    decode = Tagged <$> decode

----------------------------------------------------------------------------
-- Containers
----------------------------------------------------------------------------

instance (Bi a, Bi b) => Bi (a,b) where
    encode (a,b) = E.encodeListLen 2
                <> encode a
                <> encode b
    decode = do decodeListLenCanonicalOf 2
                !x <- decode
                !y <- decode
                return (x, y)

instance (Bi a, Bi b, Bi c) => Bi (a,b,c) where
    encode (a,b,c) = E.encodeListLen 3
                  <> encode a
                  <> encode b
                  <> encode c

    decode = do decodeListLenCanonicalOf 3
                !x <- decode
                !y <- decode
                !z <- decode
                return (x, y, z)

instance (Bi a, Bi b, Bi c, Bi d) => Bi (a,b,c,d) where
    encode (a,b,c,d) = E.encodeListLen 4
                    <> encode a
                    <> encode b
                    <> encode c
                    <> encode d

    decode = do decodeListLenCanonicalOf 4
                !a <- decode
                !b <- decode
                !c <- decode
                !d <- decode
                return (a, b, c, d)

instance Bi BS.ByteString where
    encode = E.encodeBytes
    decode = toDecoder D.decodeBytes

instance Bi Text.Text where
    encode = E.encodeString
    decode = toDecoder D.decodeString

instance Bi BS.Lazy.ByteString where
    encode = encode . BS.Lazy.toStrict
    decode = BS.Lazy.fromStrict <$> decode

instance Bi a => Bi [a] where
    encode = encodeList
    decode = decodeList

instance (Bi a, Bi b) => Bi (Either a b) where
    encode (Left  x) = E.encodeListLen 2 <> E.encodeWord 0 <> encode x
    encode (Right x) = E.encodeListLen 2 <> E.encodeWord 1 <> encode x

    decode = do
        decodeListLenCanonicalOf 2
        (t :: Word) <- decode
        case t of
            0 -> do !x <- decode
                    return (Left x)
            1 -> do !x <- decode
                    return (Right x)
            _ -> fail $ "decode@Either: unknown tag " <> show t

instance Bi a => Bi (NonEmpty a) where
    encode = defaultEncodeList . toList
    decode = do
      l <- defaultDecodeList
      case nonEmpty l of
          Nothing -> fail "Expected a NonEmpty list, but an empty list was found!"
          Just xs -> return xs

instance Bi a => Bi (Maybe a) where
    encode Nothing  = E.encodeListLen 0
    encode (Just x) = E.encodeListLen 1 <> encode x

    decode = do n <- decodeListLenCanonical
                case n of
                    0 -> return Nothing
                    1 -> do !x <- decode
                            return (Just x)
                    _ -> fail $ "decode@Maybe: unknown tag " <> show n

encodeContainerSkel :: (Word -> E.Encoding)
                    -> (container -> Int)
                    -> (accumFunc -> E.Encoding -> container -> E.Encoding)
                    -> accumFunc
                    -> container
                    -> E.Encoding
encodeContainerSkel encodeLen size foldFunction f  c =
    encodeLen (fromIntegral (size c)) <> foldFunction f mempty c
{-# INLINE encodeContainerSkel #-}

decodeContainerSkelWithReplicate
    :: Bi a
    => Decoder s Int
       -- ^ How to get the size of the container
    -> (Int -> Decoder s a -> Decoder s container)
       -- ^ replicateM for the container
    -> ([container] -> container)
       -- ^ concat for the container
    -> Decoder s container
decodeContainerSkelWithReplicate decodeLen replicateFun fromList = do
    -- Look at how much data we have at the moment and use it as the limit for
    -- the size of a single call to replicateFun. We don't want to use
    -- replicateFun directly on the result of decodeLen since this might lead to
    -- DOS attack (attacker providing a huge value for length). So if it's above
    -- our limit, we'll do manual chunking and then combine the containers into
    -- one.
    size <- decodeLen
    limit <- toDecoder D.peekAvailable
    if size <= limit
       then replicateFun size decode
       else do
           -- Take the max of limit and a fixed chunk size (note: limit can be
           -- 0). This basically means that the attacker can make us allocate a
           -- container of size 128 even though there's no actual input.
           let chunkSize = max limit 128
               (d, m) = size `divMod` chunkSize
               buildOne s = replicateFun s decode
           containers <- sequence $ buildOne m : replicate d (buildOne chunkSize)
           return $! fromList containers
{-# INLINE decodeContainerSkelWithReplicate #-}

encodeMapSkel :: (Bi k, Bi v)
              => (m -> Int)
              -> ((k -> v -> E.Encoding -> E.Encoding) -> E.Encoding -> m -> E.Encoding)
              -> m
              -> E.Encoding
encodeMapSkel size foldrWithKey =
    encodeContainerSkel
        E.encodeMapLen
        size
        foldrWithKey
        (\k v b -> encode k <> encode v <> b)
{-# INLINE encodeMapSkel #-}

-- | Checks canonicity by comparing the new key being decoded with
-- the previous one, to enfore these are sorted the correct way.
-- See: https://tools.ietf.org/html/rfc7049#section-3.9
-- "[..]The keys in every map must be sorted lowest value to highest.[...]"
decodeMapSkel :: (Ord k, Bi k, Bi v) => ([(k,v)] -> m) -> Decoder s m
decodeMapSkel fromDistinctAscList =
    toDecoder D.decodeMapLenCanonical >>= \case
        0 -> return (fromDistinctAscList [])
        n -> ifM (view dcNoCheck) (decodeNoCheck n) (decodeWithCheck n)
  where
    decodeNoCheck n = fromDistinctAscList <$> replicateM n decodeEntry
    decodeWithCheck n = do
        (firstKey, firstValue) <- decodeEntry
        fromDistinctAscList <$> decodeEntries (n - 1) firstKey [(firstKey, firstValue)]

    -- Decode a single (k,v).
    decodeEntry :: (Bi k, Bi v) => Decoder s (k,v)
    decodeEntry = do
        !k <- decode
        !v <- decode
        return (k, v)

    -- Decode all the entries, enforcing canonicity by ensuring that the
    -- previous key is smaller than the next one.
    decodeEntries :: (Bi k, Bi v, Ord k) => Int -> k -> [(k,v)] -> Decoder s [(k,v)]
    decodeEntries 0 _ acc = pure $ reverse acc
    decodeEntries !remainingPairs previousKey !acc = do
        p@(newKey, _) <- decodeEntry
        -- Order of keys needs to be strictly increasing, because otherwise it's
        -- possible to supply lists with various amount of duplicate keys which
        -- will result in the same map as long as the last value of the given
        -- key on the list is the same in all of them.
        case newKey > previousKey of
            True  -> decodeEntries (remainingPairs - 1) newKey (p : acc)
            False -> fail "Canonicity violation whilst decoding a Map!"
{-# INLINE decodeMapSkel #-}

instance (Hashable k, Ord k, Bi k, Bi v) => Bi (HM.HashMap k v) where
    encode = encodeMapSkel HM.size $ \f acc ->
        -- We need to encode the list with keys sorted in ascending order as
        -- that's the only representation we accept during decoding.
        foldr (uncurry f) acc . sortWith fst . HM.toList
    decode = decodeMapSkel HM.fromList

instance (Ord k, Bi k, Bi v) => Bi (Map k v) where
    encode = encodeMapSkel M.size M.foldrWithKey
    decode = decodeMapSkel M.fromDistinctAscList

encodeSetSkel :: Bi a
              => (s -> Int)
              -> ((a -> E.Encoding -> E.Encoding) -> E.Encoding -> s -> E.Encoding)
              -> s
              -> E.Encoding
encodeSetSkel size foldFunction =
    mappend encodeSetTag .
    encodeContainerSkel E.encodeListLen size foldFunction (\a b -> encode a <> b)
{-# INLINE encodeSetSkel #-}

-- We stitch a `258` in from of a (Hash)Set, so that tools which
-- programmatically check for canonicity can recognise it from a normal
-- array. Why 258? This will be formalised pretty soon, but IANA allocated
-- 256...18446744073709551615 to "First come, first served":
-- https://www.iana.org/assignments/cbor-tags/cbor-tags.xhtml Currently `258` is
-- the first unassigned tag and as it requires 2 bytes to be encoded, it sounds
-- like the best fit.
setTag :: Word
setTag = 258

encodeSetTag :: E.Encoding
encodeSetTag = E.encodeTag setTag

decodeSetTag :: Decoder s ()
decodeSetTag = do
    t <- toDecoder D.decodeTagCanonical
    when (t /= setTag) $
        fail ("decodeSetTag: this doesn't appear to be a Set. Found tag: " <> show t)

decodeSetSkel :: (Ord a, Bi a) => ([a] -> c) -> Decoder s c
decodeSetSkel fromDistinctAscList = do
    decodeSetTag
    decodeListLenCanonical >>= \case
        0 -> return (fromDistinctAscList [])
        n -> ifM (view dcNoCheck) (decodeNoCheck n) (decodeWithCheck n)
  where
    decodeNoCheck n =
        fromDistinctAscList <$> replicateM n decode
    decodeWithCheck n = do
        firstValue <- decode
        fromDistinctAscList <$> decodeEntries (n - 1) firstValue [firstValue]

    decodeEntries :: (Bi v, Ord v) => Int -> v -> [v] -> Decoder s [v]
    decodeEntries 0 _ acc = pure $ reverse acc
    decodeEntries !remainingEntries previousValue !acc = do
        newValue <- decode
        -- Order of values needs to be strictly increasing, because otherwise
        -- it's possible to supply lists with various amount of duplicates which
        -- will result in the same set.
        case newValue > previousValue of
            True  -> decodeEntries (remainingEntries - 1) newValue (newValue : acc)
            False -> fail "Canonicity violation whilst decoding a Set!"
{-# INLINE decodeSetSkel #-}

instance (Hashable a, Ord a, Bi a) => Bi (HashSet a) where
    encode = encodeSetSkel HS.size $ \f acc ->
        -- We need to encode the list sorted in ascending order as that's the only
        -- representation we accept during decoding.
        foldr f acc . sort . HS.toList
    decode = decodeSetSkel HS.fromList

instance (Ord a, Bi a) => Bi (Set a) where
    encode = encodeSetSkel S.size S.foldr
    decode = decodeSetSkel S.fromDistinctAscList

-- | Generic encoder for vectors. Its intended use is to allow easy
-- definition of 'Serialise' instances for custom vector
encodeVector :: (Bi a, Vector.Generic.Vector v a)
             => v a -> E.Encoding
encodeVector = encodeContainerSkel
    E.encodeListLen
    Vector.Generic.length
    Vector.Generic.foldr
    (\a b -> encode a <> b)
{-# INLINE encodeVector #-}

-- | Generic decoder for vectors. Its intended use is to allow easy
-- definition of 'Serialise' instances for custom vector
decodeVector :: (Bi a, Vector.Generic.Vector v a)
             => Decoder s (v a)
decodeVector =
    decodeContainerSkelWithReplicate
    decodeListLenCanonical
    Vector.Generic.replicateM
    Vector.Generic.concat
{-# INLINE decodeVector #-}

instance (Bi a) => Bi (Vector.Vector a) where
    encode = encodeVector
    {-# INLINE encode #-}
    decode = decodeVector
    {-# INLINE decode #-}

----------------------------------------------------------------------------
-- Other types
----------------------------------------------------------------------------

instance Bi Millisecond where
    encode = encode . toInteger
    decode = fromInteger <$> decode

instance Bi Microsecond where
    encode = encode . toInteger
    decode = fromInteger <$> decode

instance Bi Byte where
    encode = encode . toBytes
    decode = fromBytes <$> decode

----------------------------------------------------------------------------
-- Generic deriving
----------------------------------------------------------------------------

genericEncode :: (Generic a, GSerialiseEncode (G.Rep a)) => a -> E.Encoding
genericEncode = gencode . G.from

genericDecode :: (Generic a, GSerialiseDecode (G.Rep a)) => Decoder s a
genericDecode = G.to <$> gdecode

class GSerialiseEncode f where
    gencode  :: f a -> E.Encoding

class GSerialiseDecode f where
    gdecode  :: Decoder s (f a)

instance GSerialiseEncode G.V1 where
    -- Data types without constructors are still serialised as null value
    gencode _ = E.encodeNull

instance GSerialiseDecode G.V1 where
    gdecode   = error "G.V1 don't have contructors" <$ toDecoder D.decodeNull

instance GSerialiseEncode G.U1 where
    -- Constructors without fields are serialised as null value
    gencode _ = E.encodeListLen 0

instance GSerialiseDecode G.U1 where
    gdecode   = do
      n <- decodeListLenCanonical
      when (n /= 0) $ fail "expect list of length 0"
      return G.U1

instance GSerialiseEncode a => GSerialiseEncode (G.M1 i c a) where
    -- Metadata (constructor name, etc) is skipped
    gencode = gencode . G.unM1

instance GSerialiseDecode a => GSerialiseDecode (G.M1 i c a) where
    gdecode = G.M1 <$> gdecode

instance Bi a => GSerialiseEncode (G.K1 i a) where
    -- Constructor field (Could only appear in one-field & one-constructor
    -- data types). In all other cases we go through GSerialise{Sum,Prod}
    gencode (G.K1 a) = E.encodeListLen 1
                     <> encode a

instance Bi a => GSerialiseDecode (G.K1 i a) where
    gdecode = do
        n <- decodeListLenCanonical
        when (n /= 1) $
          fail "expect list of length 1"
        G.K1 <$> decode

instance (GSerialiseProd f, GSerialiseProd g) => GSerialiseEncode (f G.:*: g) where
    -- Products are serialised as N-tuples with 0 constructor tag
    gencode (f G.:*: g)
        = E.encodeListLen (nFields (Proxy :: Proxy (f G.:*: g)))
       <> encodeSeq f
       <> encodeSeq g

instance (GSerialiseProd f, GSerialiseProd g) => GSerialiseDecode (f G.:*: g) where
    gdecode = do
        let nF = nFields (Proxy :: Proxy (f G.:*: g))
        n <- decodeListLenCanonical
        -- TODO FIXME: signedness of list length
        when (fromIntegral n /= nF) $
          fail $ "Wrong number of fields: expected="++show (nF)++" got="++show n
        !f <- gdecodeSeq
        !g <- gdecodeSeq
        return $ f G.:*: g

instance (GSerialiseSum f, GSerialiseSum g) => GSerialiseEncode (f G.:+: g) where
    -- Sum types are serialised as N-tuples and first element is
    -- constructor tag
    gencode a = E.encodeListLen (numOfFields a + 1)
             <> encode (conNumber a)
             <> encodeSum a

instance (GSerialiseSum f, GSerialiseSum g) => GSerialiseDecode (f G.:+: g) where
    gdecode = do
        n <- decodeListLenCanonical
        -- TODO FIXME: Again signedness
        when (n == 0) $
          fail "Empty list encountered for sum type"
        (nCon :: Word) <- decode
        trueN <- fieldsForCon (Proxy :: Proxy (f G.:+: g)) nCon
        when (n-1 /= fromIntegral trueN ) $
          fail $ "Number of fields mismatch: expected="++show trueN++" got="++show n
        decodeSum nCon


-- | Serialization of product types
class GSerialiseProd f where
    -- | Number of fields in product type
    nFields   :: Proxy f -> Word
    -- | Encode fields sequentially without writing header
    encodeSeq :: f a -> E.Encoding
    -- | Decode fields sequentially without reading header
    gdecodeSeq :: Decoder s (f a)

instance (GSerialiseProd f, GSerialiseProd g) => GSerialiseProd (f G.:*: g) where
    nFields _ = nFields (Proxy :: Proxy f) + nFields (Proxy :: Proxy g)
    encodeSeq (f G.:*: g) = encodeSeq f <> encodeSeq g
    gdecodeSeq = do !f <- gdecodeSeq
                    !g <- gdecodeSeq
                    return (f G.:*: g)

instance GSerialiseProd G.U1 where
    -- N.B. Could only be reached when one of constructors in sum type
    --      don't have parameters
    nFields   _ = 0
    encodeSeq _ = mempty
    gdecodeSeq  = return G.U1

instance (Bi a) => GSerialiseProd (G.K1 i a) where
    -- Ordinary field
    nFields    _     = 1
    encodeSeq (G.K1 f) = encode f
    gdecodeSeq       = G.K1 <$> decode

instance (i ~ G.S, GSerialiseProd f) => GSerialiseProd (G.M1 i c f) where
    -- We skip metadata
    nFields     _     = 1
    encodeSeq  (G.M1 f) = encodeSeq f
    gdecodeSeq        = G.M1 <$> gdecodeSeq

-- | Serialization of sum types
--
class GSerialiseSum f where
    -- | Number of constructor of given value
    conNumber   :: f a -> Word
    -- | Number of fields of given value
    numOfFields :: f a -> Word
    -- | Encode field
    encodeSum   :: f a  -> E.Encoding

    -- | Decode field
    decodeSum     :: Word -> Decoder s (f a)
    -- | Number of constructors
    nConstructors :: Proxy f -> Word
    -- | Number of fields for given constructor number
    fieldsForCon  :: Proxy f -> Word -> Decoder s Word

instance (GSerialiseSum f, GSerialiseSum g) => GSerialiseSum (f G.:+: g) where
    conNumber x = case x of
      G.L1 f -> conNumber f
      G.R1 g -> conNumber g + nConstructors (Proxy :: Proxy f)
    numOfFields x = case x of
      G.L1 f -> numOfFields f
      G.R1 g -> numOfFields g
    encodeSum x = case x of
      G.L1 f -> encodeSum f
      G.R1 g -> encodeSum g

    nConstructors _ = nConstructors (Proxy :: Proxy f)
                    + nConstructors (Proxy :: Proxy g)

    fieldsForCon _ n | n < nL    = fieldsForCon (Proxy :: Proxy f) n
                     | otherwise = fieldsForCon (Proxy :: Proxy g) (n - nL)
      where
        nL = nConstructors (Proxy :: Proxy f)

    decodeSum nCon | nCon < nL = G.L1 <$> decodeSum nCon
                   | otherwise = G.R1 <$> decodeSum (nCon - nL)
      where
        nL = nConstructors (Proxy :: Proxy f)

instance (i ~ G.C, GSerialiseProd f) => GSerialiseSum (G.M1 i c f) where
    conNumber    _     = 0
    numOfFields  _     = nFields (Proxy :: Proxy f)
    encodeSum   (G.M1 f) = encodeSeq f

    nConstructors  _ = 1
    fieldsForCon _ 0 = return $ nFields (Proxy :: Proxy f)
    fieldsForCon _ _ = fail "Bad constructor number"
    decodeSum      0 = G.M1 <$> gdecodeSeq
    decodeSum      _ = fail "bad constructor number"
