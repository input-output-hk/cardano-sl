{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE CPP              #-}

-- | Bi typeclass and most basic functions.

#include "MachDeps.h"

module Pos.Binary.Class.Core
    ( Bi(..)
    , encodeBinary
    , decodeBinary
    , enforceSize
    , matchSize
    -- * CBOR re-exports
    , E.encodeListLen
    , D.decodeListLenCanonical
    , D.decodeListLenCanonicalOf
    , E.Encoding
    , D.Decoder
    , CBOR.Read.deserialiseIncremental
    , CBOR.Write.toLazyByteString
    , CBOR.Write.toBuilder
    , CBOR.Read.IDecode(..)
    -- * GHC-Generics-based encoding & decoding
    , genericEncode
    , genericDecode
    -- * Utils
    , toCborError
    , cborError
    , withWordSize
    
    , Range(..)
    , szEval
    , Size
    , Length(..)
    , isTodo
    , szCases
    , szLazy
    , szGreedy
    , force1
    , szWithCtx
    , simplify
    , pp
    , apMono
    ) where

import           Universum
import           Formatting (bprint, build, shown, string, (%))

import qualified Codec.CBOR.Decoding as D
import qualified Codec.CBOR.Encoding as E
import qualified Codec.CBOR.Read as CBOR.Read
import qualified Codec.CBOR.Write as CBOR.Write
import qualified Data.Binary as Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BS.Lazy
import qualified Data.Char as Char
import           Data.Coerce
import           Data.Fixed (Fixed (..), Nano)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Map as M
import qualified Data.Set as S
import           Data.Tagged (Tagged (..))
import qualified Data.Text as Text
import qualified Data.Text.Buildable
import           Data.Time.Units (Microsecond, Millisecond)
import           Data.Typeable (typeRep, TypeRep)
import qualified Data.Vector as Vector
import qualified Data.Vector.Generic as Vector.Generic
import           Foreign.Storable (sizeOf)
import qualified GHC.Generics as G
import           Serokell.Data.Memory.Units (Byte, fromBytes, toBytes)

-- | This function must match the one from 'Pos.Util.Util'. It is copied here
-- to avoid a dependency and facilitate parallel builds.
toCborError :: Either Text a -> D.Decoder s a
toCborError = either (fail . toString) return

cborError :: Text -> D.Decoder s a
cborError = toCborError . Left

encodeBinary :: Binary.Binary a => a -> E.Encoding
encodeBinary = encode . BS.Lazy.toStrict . Binary.encode

decodeBinary :: Binary.Binary a => D.Decoder s a
decodeBinary = do
    x <- decode @ByteString
    toCborError $ case Binary.decodeOrFail (BS.Lazy.fromStrict x) of
        Left (_, _, err) -> Left (fromString err)
        Right (bs, _, res)
            | BS.Lazy.null bs -> Right res
            | otherwise       -> Left "decodeBinary: unconsumed input"

-- | Enforces that the input size is the same as the decoded one, failing in
-- case it's not.
enforceSize :: Text -> Int -> D.Decoder s ()
enforceSize lbl requestedSize = D.decodeListLenCanonical >>= matchSize requestedSize lbl

-- | Compare two sizes, failing if they are not equal.
matchSize :: Int -> Text -> Int -> D.Decoder s ()
matchSize requestedSize lbl actualSize =
  when (actualSize /= requestedSize) $
    cborError (lbl <> " failed the size check. Expected " <> show requestedSize <> ", found " <> show actualSize)

withSize :: Integral s => s -> a -> a -> a -> a -> a -> a
withSize s a1 a2 a3 a4 a5 =
    if | s <= 0x17 -> a1
       | s <= 0xff -> a2
       | s <= 0xffff -> a3
#if WORD_SIZE_IN_BITS == 64
       | s <= 0xffffffff -> a4
       | otherwise -> a5
#elif WORD_SIZE_IN_BITS == 32
       | otherwise -> a4
#define ARCH_32bit
#else
#error expected WORD_SIZE_IN_BITS to be 32 or 64
#endif

-- | Compute size of a @'Word'@, this is used for computation of @'Word8'@, ..,
-- @'Word32'@ encoded size.
withWordSize :: (Integral s, Integral a) => s -> a
withWordSize s = withSize s 1 2 3 5 9

----------------------------------------

class Typeable a => Bi a where
    encode :: a -> E.Encoding
    decode :: D.Decoder s a

    label :: Proxy a -> Text
    label = show . typeRep

    encodeList :: [a] -> E.Encoding
    encodeList = defaultEncodeList

    decodeList :: D.Decoder s [a]
    decodeList = defaultDecodeList

    encodedSizeExpr :: (forall t. Bi t => Proxy t -> Size) -> Proxy a -> Size
    encodedSizeExpr = todo

    encodedListSizeExpr :: (forall t. Bi t => Proxy t -> Size) -> Proxy [a] -> Size
    encodedListSizeExpr = defaultEncodedListSizeExpr
    
-- | Default @'E.Encoding'@ for list types.
defaultEncodeList :: Bi a => [a] -> E.Encoding
defaultEncodeList xs = E.encodeListLenIndef
                    <> Universum.foldr (\x r -> encode x <> r) E.encodeBreak xs

-- | Default @'D.Decoder'@ for list types.
defaultDecodeList :: Bi a => D.Decoder s [a]
defaultDecodeList = do
    D.decodeListLenIndef
    D.decodeSequenceLenIndef (flip (:)) [] reverse decode

newtype Length xs = Length xs deriving Typeable
instance Typeable xs => Bi (Length xs) where
  encode = error "The `Length` type cannot be encoded!"
  decode = error "The `Length` type cannot be decoded!"

-- | Default size expression for a value.
defaultEncodedListSizeExpr :: forall a. Bi a => (forall t. Bi t => Proxy t -> Size) -> Proxy [a] -> Size
defaultEncodedListSizeExpr size _ = 2 + size (Proxy @(Length [a])) * size (Proxy @a)
  

----------------------------------------------------------------------------
-- Primitive types
----------------------------------------------------------------------------

instance Bi () where
    encode = const E.encodeNull
    decode = D.decodeNull
    encodedSizeExpr _ _ = 1
    
instance Bi Bool where
    encode = E.encodeBool
    decode = D.decodeBool
    encodedSizeExpr _ _ = 1
    
instance Bi Char where
    encode c = E.encodeString (Text.singleton c)
    decode = do t <- D.decodeStringCanonical
                toCborError $ if Text.length t == 1
                  then Right (Text.head t)
                  else Left "expected a single char, found a string"

    -- For [Char]/String we have a special encoding
    encodeList cs = E.encodeString (toText cs)
    decodeList    = do txt <- D.decodeStringCanonical
                       return (toString txt) -- unpack lazily

    encodedSizeExpr _ pxy = encodedSizeRange (Char.ord <$> pxy)
    encodedListSizeExpr size _ =
        let bsLength = size (Proxy @(Length [Char])) * size (Proxy @Char)
        in  bsLength + apMono "(withSize _ 1 2 3 4 5)" (\x -> withSize x 1 2 3 4 5) bsLength

----------------------------------------------------------------------------
-- Numeric data
----------------------------------------------------------------------------

instance Bi Integer where
    encode = E.encodeInteger
    decode = D.decodeIntegerCanonical

encodedSizeRange :: forall a. (Integral a, Bounded a) => Proxy a -> Size
encodedSizeRange _ = szCases $
  map (fromIntegral . (withWordSize :: a -> Integer)) [ minBound, maxBound ]

instance Bi Word where
    encode = E.encodeWord
    decode = D.decodeWordCanonical
    encodedSizeExpr _ w = encodedSizeRange w
    
instance Bi Word8 where
    encode = E.encodeWord8
    decode = D.decodeWord8Canonical
    encodedSizeExpr _ w = encodedSizeRange w
    
instance Bi Word16 where
    encode = E.encodeWord16
    decode = D.decodeWord16Canonical
    encodedSizeExpr _ w = encodedSizeRange w
    
instance Bi Word32 where
    encode = E.encodeWord32
    decode = D.decodeWord32Canonical
    encodedSizeExpr _ w = encodedSizeRange w
    
instance Bi Word64 where
    encode = E.encodeWord64
    decode = D.decodeWord64Canonical
    encodedSizeExpr _ w = encodedSizeRange w

instance Bi Int where
    encode = E.encodeInt
    decode = D.decodeIntCanonical
    encodedSizeExpr _ w = encodedSizeRange w

instance Bi Float where
    encode = E.encodeFloat
    decode = D.decodeFloatCanonical
    encodedSizeExpr _ _ = 1 + fromIntegral (sizeOf (0 :: Float))

instance Bi Int32 where
    encode = E.encodeInt32
    decode = D.decodeInt32Canonical
    encodedSizeExpr _ w = encodedSizeRange w
    
instance Bi Int64 where
    encode = E.encodeInt64
    decode = D.decodeInt64Canonical
    encodedSizeExpr _ w = encodedSizeRange w

instance Bi Nano where
    encode (MkFixed resolution) = encode resolution
    decode = MkFixed <$> decode

instance Bi Void where
    decode = cborError "instance Bi Void: you shouldn't try to deserialize Void"
    encode = absurd

----------------------------------------------------------------------------
-- Tagged
----------------------------------------------------------------------------

instance (Typeable s, Bi a) => Bi (Tagged s a) where
    encode (Tagged a) = encode a
    decode = Tagged <$> decode
    encodedSizeExpr size _ = encodedSizeExpr size (Proxy @a)

----------------------------------------------------------------------------
-- Containers
----------------------------------------------------------------------------

instance (Bi a, Bi b) => Bi (a,b) where
    encode (a,b) = E.encodeListLen 2
                <> encode a
                <> encode b
    decode = do D.decodeListLenCanonicalOf 2
                !x <- decode
                !y <- decode
                return (x, y)
    encodedSizeExpr size _ = 1 + size (Proxy @a) + size (Proxy @b)

instance (Bi a, Bi b, Bi c) => Bi (a,b,c) where
    encode (a,b,c) = E.encodeListLen 3
                  <> encode a
                  <> encode b
                  <> encode c

    decode = do D.decodeListLenCanonicalOf 3
                !x <- decode
                !y <- decode
                !z <- decode
                return (x, y, z)

    encodedSizeExpr size _ = 1 + size (Proxy @a) + size (Proxy @b) + size (Proxy @c)

instance (Bi a, Bi b, Bi c, Bi d) => Bi (a,b,c,d) where
    encode (a,b,c,d) = E.encodeListLen 4
                    <> encode a
                    <> encode b
                    <> encode c
                    <> encode d

    decode = do D.decodeListLenCanonicalOf 4
                !a <- decode
                !b <- decode
                !c <- decode
                !d <- decode
                return (a, b, c, d)

    encodedSizeExpr size _ = 1 + size (Proxy @a) + size (Proxy @b) + size (Proxy @c) + size (Proxy @d)

instance Bi BS.ByteString where
    encode = E.encodeBytes
    decode = D.decodeBytesCanonical
    encodedSizeExpr size _ = let len = size (Proxy @(Length BS.ByteString))
        in apMono "withWordSize@Int" (withWordSize @Int . fromIntegral) len + len

instance Bi Text.Text where
    encode = E.encodeString
    decode = D.decodeStringCanonical
    encodedSizeExpr size _ = encodedSizeExpr size (Proxy @[Char])

instance Bi BS.Lazy.ByteString where
    encode = encode . BS.Lazy.toStrict
    decode = BS.Lazy.fromStrict <$> decode
    encodedSizeExpr size _ = let len = size (Proxy @(Length BS.Lazy.ByteString))
        in apMono "withWordSize@Int" (withWordSize @Int . fromIntegral) len + len

instance Bi a => Bi [a] where
    encode = encodeList
    decode = decodeList
    encodedSizeExpr size _ = encodedListSizeExpr size (Proxy @[a])

instance (Bi a, Bi b) => Bi (Either a b) where
    encode (Left  x) = E.encodeListLen 2 <> E.encodeWord 0 <> encode x
    encode (Right x) = E.encodeListLen 2 <> E.encodeWord 1 <> encode x

    decode = do D.decodeListLenCanonicalOf 2
                t <- D.decodeWordCanonical
                case t of
                  0 -> do !x <- decode
                          return (Left x)
                  1 -> do !x <- decode
                          return (Right x)
                  _ -> cborError $ "decode@Either: unknown tag " <> show t

    encodedSizeExpr size _ = 2 + szCases [size (Proxy @a), size (Proxy @b)]

instance Bi a => Bi (NonEmpty a) where
    encode = defaultEncodeList . toList
    decode =
        nonEmpty <$> defaultDecodeList >>= toCborError . \case
            Nothing -> Left "Expected a NonEmpty list, but an empty list was found!"
            Just xs -> Right xs
    encodedSizeExpr size _ = size (Proxy @[a]) -- MN TODO make 0 count impossible

instance Bi a => Bi (Maybe a) where
    encode Nothing  = E.encodeListLen 0
    encode (Just x) = E.encodeListLen 1 <> encode x

    decode = do n <- D.decodeListLenCanonical
                case n of
                  0 -> return Nothing
                  1 -> do !x <- decode
                          return (Just x)
                  _ -> cborError $ "decode@Maybe: unknown tag " <> show n

    encodedSizeExpr size _ = 1 + szCases [0, size (Proxy @a)]

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
  => D.Decoder s Int
     -- ^ How to get the size of the container
  -> (Int -> D.Decoder s a -> D.Decoder s container)
     -- ^ replicateM for the container
  -> ([container] -> container)
     -- ^ concat for the container
  -> D.Decoder s container
decodeContainerSkelWithReplicate decodeLen replicateFun fromList = do
    -- Look at how much data we have at the moment and use it as the limit for
    -- the size of a single call to replicateFun. We don't want to use
    -- replicateFun directly on the result of decodeLen since this might lead to
    -- DOS attack (attacker providing a huge value for length). So if it's above
    -- our limit, we'll do manual chunking and then combine the containers into
    -- one.
    size <- decodeLen
    limit <- D.peekAvailable
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
decodeMapSkel :: (Ord k, Bi k, Bi v) => ([(k,v)] -> m) -> D.Decoder s m
decodeMapSkel fromDistinctAscList = do
    n <- D.decodeMapLenCanonical
    case n of
        0 -> return (fromDistinctAscList [])
        _ -> do
            (firstKey, firstValue) <- decodeEntry
            fromDistinctAscList <$> decodeEntries (n - 1) firstKey [(firstKey, firstValue)]
  where
    -- Decode a single (k,v).
    decodeEntry :: (Bi k, Bi v) => D.Decoder s (k,v)
    decodeEntry = do
        !k <- decode
        !v <- decode
        return (k, v)

    -- Decode all the entries, enforcing canonicity by ensuring that the
    -- previous key is smaller than the next one.
    decodeEntries :: (Bi k, Bi v, Ord k) => Int -> k -> [(k,v)] -> D.Decoder s [(k,v)]
    decodeEntries 0 _ acc = pure $ reverse acc
    decodeEntries !remainingPairs previousKey !acc = do
        p@(newKey, _) <- decodeEntry
        -- Order of keys needs to be strictly increasing, because otherwise it's
        -- possible to supply lists with various amount of duplicate keys which
        -- will result in the same map as long as the last value of the given
        -- key on the list is the same in all of them.
        case newKey > previousKey of
            True  -> decodeEntries (remainingPairs - 1) newKey (p : acc)
            False -> cborError "Canonicity violation whilst decoding a Map!"
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

decodeSetTag :: D.Decoder s ()
decodeSetTag = do
    t <- D.decodeTagCanonical
    when (t /= setTag) $ cborError ("decodeSetTag: this doesn't appear to be a Set. Found tag: " <> show t)

decodeSetSkel :: (Ord a, Bi a) => ([a] -> c) -> D.Decoder s c
decodeSetSkel fromDistinctAscList = do
    decodeSetTag
    n <- D.decodeListLenCanonical
    case n of
        0 -> return (fromDistinctAscList [])
        _ -> do
            firstValue <- decode
            fromDistinctAscList <$> decodeEntries (n - 1) firstValue [firstValue]
  where
    decodeEntries :: (Bi v, Ord v) => Int -> v -> [v] -> D.Decoder s [v]
    decodeEntries 0 _ acc = pure $ reverse acc
    decodeEntries !remainingEntries previousValue !acc = do
        newValue <- decode
        -- Order of values needs to be strictly increasing, because otherwise
        -- it's possible to supply lists with various amount of duplicates which
        -- will result in the same set.
        case newValue > previousValue of
            True  -> decodeEntries (remainingEntries - 1) newValue (newValue : acc)
            False -> cborError "Canonicity violation whilst decoding a Set!"
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
             => D.Decoder s (v a)
decodeVector = decodeContainerSkelWithReplicate
    D.decodeListLenCanonical
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

genericDecode :: (Generic a, GSerialiseDecode (G.Rep a)) => D.Decoder s a
genericDecode = G.to <$> gdecode

class GSerialiseEncode f where
    gencode  :: f a -> E.Encoding

class GSerialiseDecode f where
    gdecode  :: D.Decoder s (f a)

instance GSerialiseEncode G.V1 where
    -- Data types without constructors are still serialised as null value
    gencode _ = E.encodeNull

instance GSerialiseDecode G.V1 where
    gdecode   = error "G.V1 don't have contructors" <$ D.decodeNull

instance GSerialiseEncode G.U1 where
    -- Constructors without fields are serialised as null value
    gencode _ = E.encodeListLen 0

instance GSerialiseDecode G.U1 where
    gdecode   = do
      n <- D.decodeListLenCanonical
      when (n /= 0) $ cborError "expect list of length 0"
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
      n <- D.decodeListLenCanonical
      when (n /= 1) $
        cborError "expect list of length 1"
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
      n <- D.decodeListLenCanonical
      -- TODO FIXME: signedness of list length
      when (fromIntegral n /= nF) $
        cborError $ "Wrong number of fields: expected="<>show (nF)<>" got="<>show n
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
        n <- D.decodeListLenCanonical
        -- TODO FIXME: Again signedness
        when (n == 0) $
          cborError "Empty list encountered for sum type"
        nCon  <- D.decodeWordCanonical
        trueN <- fieldsForCon (Proxy :: Proxy (f G.:+: g)) nCon
        when (n-1 /= fromIntegral trueN ) $
          cborError $ "Number of fields mismatch: expected="<>show trueN<>" got="<>show n
        decodeSum nCon


-- | Serialization of product types
class GSerialiseProd f where
    -- | Number of fields in product type
    nFields   :: Proxy f -> Word
    -- | Encode fields sequentially without writing header
    encodeSeq :: f a -> E.Encoding
    -- | Decode fields sequentially without reading header
    gdecodeSeq :: D.Decoder s (f a)

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
    decodeSum     :: Word -> D.Decoder s (f a)
    -- | Number of constructors
    nConstructors :: Proxy f -> Word
    -- | Number of fields for given constructor number
    fieldsForCon  :: Proxy f -> Word -> D.Decoder s Word

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
    fieldsForCon _ _ = cborError "Bad constructor number"
    decodeSum      0 = G.M1 <$> gdecodeSeq
    decodeSum      _ = cborError "bad constructor number"


----------------------------------------------------------------------------
-- Size expressions
----------------------------------------------------------------------------

(.:) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
f .: g = \x y -> f (g x y)

data SizeF t
  = AddF t t
  | MulF t t
  | SubF t t
  | AbsF t
  | NegF t
  | SgnF t
  | CasesF [t]
  | ValueF Byte
  | ApF String (Byte -> Byte) t
  | forall a. Bi a => TodoF (forall x. Bi x => Proxy x -> Size) (Proxy a)
  deriving Typeable

instance Functor SizeF where
  fmap f = \case
    AddF x y  -> AddF (f x) (f y)
    MulF x y  -> MulF (f x) (f y)
    SubF x y  -> SubF (f x) (f y)
    AbsF x    -> AbsF (f x)
    NegF x    -> NegF (f x)
    SgnF x    -> SgnF (f x)
    CasesF xs -> CasesF (map f xs)
    ValueF x  -> ValueF x
    ApF n g x -> ApF n g (f x)
    TodoF g x -> TodoF g x

cata :: Functor f => (f t -> t) -> Fix f -> t
cata phi x = phi (cata phi <$> unfix x)

ana :: Functor f => (t -> f t) -> t -> Fix f
ana phi x = Fix (ana phi <$> phi x)

newtype Fix f = Fix (f (Fix f))

unfix :: Fix f -> f (Fix f)
unfix = coerce

type Size = Fix SizeF

instance Num (Fix SizeF) where
  (+) = Fix .: AddF
  (*) = Fix .: MulF
  (-) = Fix .: SubF
  negate = Fix . NegF
  abs    = Fix . AbsF
  signum = Fix . SgnF
  fromInteger = Fix . ValueF . fromInteger

instance Buildable t => Buildable (SizeF t) where
  build x_ =
    let showp2 c x y = bprint ("(" % build % " " % string % " " % build % ")") x c y
    in case x_ of
         AddF x y -> showp2 "+" x y
         MulF x y -> showp2 "*" x y
         SubF x y -> showp2 "-" x y
         NegF x   -> bprint ("-" % build) x
         AbsF x   -> bprint ("|" % build % "|") x
         SgnF x   -> bprint ("sgn(" % build % ")") x
         CasesF xs -> bprint ("{ " % build % "}") $ foldMap (bprint (build % " ")) xs
         ValueF x  -> bprint shown (toInteger x)
         ApF n _ x -> bprint (string % "(" % build % ")") n x
         TodoF _ x -> bprint ("(_ :: " % shown % ")") (typeRep x)

instance Buildable (Fix SizeF) where
  build x = bprint build (unfix x)

szCases :: [Size] -> Size
szCases = Fix . CasesF

data Range b = Range { lo :: b, hi :: b }

instance Num b => Num (Range b) where
  x + y = Range { lo = lo x + lo y
                , hi = hi x + hi y }
  x * y = Range { lo = lo x * lo y
                , hi = hi x * hi y }
  x - y = Range { lo = lo x - hi y
                , hi = hi x - lo y }
  negate x = Range { lo = negate (hi x)
                   , hi = negate (lo x) }
  abs x = x
  signum x = x
  fromInteger n = Range { lo = fromInteger n
                        , hi = fromInteger n }

instance Buildable (Range Byte) where
  build r = bprint (shown % ".." % shown) (toInteger $ lo r) (toInteger $ hi r)

instance Buildable (Either Size (Range Byte)) where
  build (Right x) = bprint build x
  build (Left x)  = bprint build x

szEval :: (forall t. Bi t => (Proxy t -> Size) -> Proxy t -> Range Byte) -> Size -> Range Byte
szEval doit = cata $ \case
  AddF x y -> x + y
  MulF x y -> x * y
  SubF x y -> x - y
  NegF x   -> negate x
  AbsF x   -> abs x
  SgnF x   -> signum x
  CasesF xs -> Range { lo = minimum (map lo xs)
                    , hi = maximum (map hi xs) }
  ValueF x -> Range { lo = x, hi = x }
  ApF _ f x -> Range { lo = f (lo x), hi = f (hi x) }
  TodoF f x -> doit f x

szLazy :: Bi a => (Proxy a -> Size)
szLazy = todo (encodedSizeExpr szLazy)

szGreedy :: Bi a => (Proxy a -> Size)
szGreedy = encodedSizeExpr szGreedy

isTodo :: Size -> Bool
isTodo (Fix (TodoF _ _)) = True
isTodo _ = False

todo :: forall a. Bi a => (forall t. Bi t => Proxy t -> Size) -> Proxy a -> Size
todo f pxy = Fix (TodoF f pxy)

pp :: Buildable a => a -> IO ()
pp = putStrLn . pretty

apMono :: String -> (Byte -> Byte) -> Size -> Size
apMono n f = \case
  Fix (ValueF x)  -> Fix (ValueF (f x))
  Fix (CasesF cs) -> Fix (CasesF (map (apMono n f) cs))
  x               -> Fix (ApF n f x)

szWithCtx :: Bi a => Map TypeRep Size -> Proxy a -> Size
szWithCtx ctx pxy = case M.lookup (typeRep pxy) ctx of
  Just ans -> ans
  Nothing  ->  encodedSizeExpr (szWithCtx ctx) pxy

simplify :: Size -> Either Size (Range Byte)
simplify = cata $ \case
    TodoF f pxy -> Left (todo f pxy)
    ValueF x    -> Right (Range { lo = x, hi = x })
    CasesF xs   -> case sequence xs of
                    Right xs' -> Right (Range { lo = minimum (map lo xs')
                                              , hi = maximum (map hi xs') })
                    Left _  -> Left (szCases $ map toSize xs)
    AddF x y  -> binOp (+) x y
    MulF x y  -> binOp (*) x y
    SubF x y  -> binOp (-) x y
    NegF x    -> unOp negate x
    AbsF x    -> unOp abs x
    SgnF x    -> unOp signum x
    ApF _ f (Right x) -> Right (Range { lo = f (lo x), hi = f (hi x) })
    ApF n f (Left x)  -> Left  (apMono n f x)

  where
    binOp :: (forall a. Num a => a -> a -> a) -> Either Size (Range Byte) -> Either Size (Range Byte) -> Either Size (Range Byte)
    binOp (#) (Right x) (Right y) = Right (x # y)
    binOp (#) x y = Left (toSize x # toSize y)

    unOp :: (forall a. Num a => a -> a) -> Either Size (Range Byte) -> Either Size (Range Byte)
    unOp f = \case
      Right x -> Right (f x)
      Left x  -> Left (f x)

    toSize :: Either Size (Range Byte) -> Size
    toSize = \case
      Left x  -> x
      Right r -> if lo r == hi r
                 then fromIntegral (lo r)
                 else szCases [fromIntegral (lo r), fromIntegral (hi r)]

force1 :: Size -> Size
force1 = cata $ \case
  AddF x y -> x + y
  MulF x y -> x * y
  SubF x y -> x - y
  NegF x   -> negate x
  AbsF x   -> abs x
  SgnF x   -> signum x
  CasesF xs -> Fix $ CasesF xs
  ValueF x  -> Fix (ValueF x)
  ApF n f x -> apMono n f x
  TodoF f x -> f x
