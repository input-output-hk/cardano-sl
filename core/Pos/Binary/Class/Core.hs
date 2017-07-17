{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module Pos.Binary.Class.Core
    ( Bi(..)
    , encodeBinary
    , decodeBinary
    , enforceSize
    , matchSize
    -- * CBOR re-exports
    , encodeListLen
    , decodeListLen
    , decodeListLenOf
    , Encoding
    , Decoder
    -- * GHC-Generics-based encoding & decoding
    , genericEncode
    , genericDecode
    ) where

import           Codec.CBOR.Decoding
import           Codec.CBOR.Encoding
import           GHC.Generics
import qualified Data.Binary                 as Binary
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Lazy        as BS.Lazy
import           Data.Fixed                  (Fixed(..), Nano)
import qualified Data.HashMap.Strict         as HM
import qualified Data.HashSet                as HS
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import           Data.Tagged
import qualified Data.Text                   as Text
import           Data.Time.Units             (Microsecond, Millisecond)
import qualified Data.Vector                 as Vector
import qualified Data.Vector.Generic         as Vector.Generic
import           Serokell.Data.Memory.Units  (Byte, fromBytes, toBytes)
import           Universum                   hiding (foldr)
import qualified Universum

encodeBinary :: Binary.Binary a => a -> Encoding
encodeBinary = encode . BS.Lazy.toStrict . Binary.encode

decodeBinary :: Binary.Binary a => Decoder s a
decodeBinary = do
    x <- decode @ByteString
    case Binary.decodeOrFail (BS.Lazy.fromStrict x) of
        Left (_, _, err) -> fail err
        Right (bs, _, res)
            | BS.Lazy.null bs -> pure res
            | otherwise       -> fail "decodeBinary: unconsumed input"

-- | Enforces that the input size is the same as the decoded one, failing in case it's not.
enforceSize :: String -> Int -> Decoder s ()
enforceSize label requestedSize = decodeListLen >>= matchSize requestedSize label

-- | Compare two sizes, failing if they are not equal.
matchSize :: Int -> String -> Int -> Decoder s ()
matchSize requestedSize label actualSize = do
  case actualSize == requestedSize of
    True  -> return ()
    False -> fail (label <> " failed the size check. Expected " <> show requestedSize <> ", found " <> show actualSize)

----------------------------------------

class Bi a where
    encode :: a -> Encoding
    decode :: Decoder s a

    encodeList :: [a] -> Encoding
    encodeList = defaultEncodeList

    decodeList :: Decoder s [a]
    decodeList = defaultDecodeList

-- | Default @'Encoding'@ for list types.
defaultEncodeList :: Bi a => [a] -> Encoding
defaultEncodeList xs = encodeListLenIndef
                    <> Universum.foldr (\x r -> encode x <> r) encodeBreak xs

-- | Default @'Decoder'@ for list types.
defaultDecodeList :: Bi a => Decoder s [a]
defaultDecodeList = do
    decodeListLenIndef
    decodeSequenceLenIndef (flip (:)) [] reverse decode

----------------------------------------------------------------------------
-- Primitive types
----------------------------------------------------------------------------

instance Bi () where
    encode = const encodeNull
    decode = decodeNull

instance Bi Bool where
    encode = encodeBool
    decode = decodeBool

instance Bi Char where
    encode c = encodeString (Text.singleton c)
    decode = do t <- decodeString
                if Text.length t == 1
                  then return $! Text.head t
                  else fail "expected a single char, found a string"

    -- For [Char]/String we have a special encoding
    encodeList cs = encodeString (Text.pack cs)
    decodeList    = do txt <- decodeString
                       return (Text.unpack txt) -- unpack lazily

----------------------------------------------------------------------------
-- Numeric data
----------------------------------------------------------------------------

instance Bi Integer where
    encode = encodeInteger
    decode = decodeInteger

instance Bi Word where
    encode = encodeWord
    decode = decodeWord

instance Bi Word8 where
    encode = encodeWord8
    decode = decodeWord8

instance Bi Word16 where
    encode = encodeWord16
    decode = decodeWord16

instance Bi Word32 where
    encode = encodeWord32
    decode = decodeWord32

instance Bi Word64 where
    encode = encodeWord64
    decode = decodeWord64

instance Bi Int where
    encode = encodeInt
    decode = decodeInt

instance Bi Float where
    encode = encodeFloat
    decode = decodeFloat

instance Bi Int32 where
    encode = encodeInt32
    decode = decodeInt32

instance Bi Int64 where
    encode = encodeInt64
    decode = decodeInt64

instance Bi Nano where
    encode (MkFixed resolution) = encodeInteger resolution
    decode = MkFixed <$> decodeInteger

instance Bi Void where
    decode = fail "instance Bi Void: you shouldn't try to deserialize Void"
    encode = error "instance Bi Void: you shouldn't try to serialize Void"

----------------------------------------------------------------------------
-- Tagged
----------------------------------------------------------------------------

instance Bi a => Bi (Tagged s a) where
    encode (Tagged a) = encode a
    decode = Tagged <$> decode

----------------------------------------------------------------------------
-- Containers
----------------------------------------------------------------------------

instance (Bi a, Bi b) => Bi (a,b) where
    encode (a,b) = encodeListLen 2
                <> encode a
                <> encode b
    decode = do decodeListLenOf 2
                !x <- decode
                !y <- decode
                return (x, y)

instance (Bi a, Bi b, Bi c) => Bi (a,b,c) where
    encode (a,b,c) = encodeListLen 3
                  <> encode a
                  <> encode b
                  <> encode c

    decode = do decodeListLenOf 3
                !x <- decode
                !y <- decode
                !z <- decode
                return (x, y, z)

instance (Bi a, Bi b, Bi c, Bi d) => Bi (a,b,c,d) where
    encode (a,b,c,d) = encodeListLen 4
                    <> encode a
                    <> encode b
                    <> encode c
                    <> encode d

    decode = do decodeListLenOf 4
                !a <- decode
                !b <- decode
                !c <- decode
                !d <- decode
                return (a, b, c, d)

instance Bi BS.ByteString where
    encode = encodeBytes
    decode = decodeBytes

instance Bi Text.Text where
    encode = encodeString
    decode = decodeString

instance Bi BS.Lazy.ByteString where
    encode = encode . BS.Lazy.toStrict
    decode = BS.Lazy.fromStrict <$> decode

instance Bi a => Bi [a] where
    encode = encodeList
    decode = decodeList

instance (Bi a, Bi b) => Bi (Either a b) where
    encode (Left  x) = encodeListLen 2 <> encodeWord 0 <> encode x
    encode (Right x) = encodeListLen 2 <> encodeWord 1 <> encode x

    decode = do decodeListLenOf 2
                t <- decodeWord
                case t of
                  0 -> do !x <- decode
                          return (Left x)
                  1 -> do !x <- decode
                          return (Right x)
                  _ -> fail "unknown tag"

instance Bi a => Bi (NonEmpty a) where
  encode = defaultEncodeList . toList
  decode = do
    l <- defaultDecodeList
    case nonEmpty l of
      Nothing -> fail "Expected a NonEmpty list, but an empty list was found!"
      Just xs -> return xs

instance Bi a => Bi (Maybe a) where
    encode Nothing  = encodeListLen 0
    encode (Just x) = encodeListLen 1 <> encode x

    decode = do n <- decodeListLen
                case n of
                  0 -> return Nothing
                  1 -> do !x <- decode
                          return (Just x)
                  _ -> fail "unknown tag"

encodeContainerSkel :: (Word -> Encoding)
                    -> (container -> Int)
                    -> (accumFunc -> Encoding -> container -> Encoding)
                    -> accumFunc
                    -> container
                    -> Encoding
encodeContainerSkel encodeLen size foldr f  c =
    encodeLen (fromIntegral (size c)) <> foldr f mempty c
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
    limit <- peekAvailable
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
              -> ((k -> v -> Encoding -> Encoding) -> Encoding -> m -> Encoding)
              -> m
              -> Encoding
encodeMapSkel size foldrWithKey =
  encodeContainerSkel
    encodeMapLen
    size
    foldrWithKey
    (\k v b -> encode k <> encode v <> b)
{-# INLINE encodeMapSkel #-}

decodeMapSkel :: (Bi k, Bi v) => ([(k,v)] -> m) -> Decoder s m
decodeMapSkel fromList = do
  n <- decodeMapLen
  let decodeEntry = do
        !k <- decode
        !v <- decode
        return (k, v)
  fmap fromList (replicateM n decodeEntry)
{-# INLINE decodeMapSkel #-}

instance (Hashable k, Ord k, Bi k, Bi v) => Bi (HM.HashMap k v) where
  encode = encodeMapSkel HM.size $ \f acc ->
      -- We need to encode the list with keys sorted in ascending order as
      -- that's the only representation we accept during decoding.
      Universum.foldr (uncurry f) acc . sortBy (comparing fst) . HM.toList
  decode = decodeMapSkel HM.fromList

instance (Ord k, Bi k, Bi v) => Bi (Map k v) where
  encode = encodeMapSkel M.size M.foldrWithKey
  decode = decodeMapSkel M.fromAscList

encodeSetSkel :: Bi a
              => (s -> Int)
              -> ((a -> Encoding -> Encoding) -> Encoding -> s -> Encoding)
              -> s
              -> Encoding
encodeSetSkel size foldr =
    encodeContainerSkel encodeListLen size foldr (\a b -> encode a <> b)
{-# INLINE encodeSetSkel #-}

decodeSetSkel :: Bi a => ([a] -> c) -> Decoder s c
decodeSetSkel fromList = do
  n <- decodeListLen
  fmap fromList (replicateM n decode)
{-# INLINE decodeSetSkel #-}

instance (Hashable a, Ord a, Bi a) => Bi (HashSet a) where
  encode = encodeSetSkel HS.size $ \f acc ->
      -- We need to encode the list sorted in ascending order as that's the only
      -- representation we accept during decoding.
      Universum.foldr f acc . sort . HS.toList
  decode = decodeSetSkel HS.fromList

instance (Ord a, Bi a) => Bi (Set a) where
  encode = encodeSetSkel S.size S.foldr
  decode = decodeSetSkel S.fromAscList

-- | Generic encoder for vectors. Its intended use is to allow easy
-- definition of 'Serialise' instances for custom vector
encodeVector :: (Bi a, Vector.Generic.Vector v a)
             => v a -> Encoding
encodeVector = encodeContainerSkel
    encodeListLen
    Vector.Generic.length
    Vector.Generic.foldr
    (\a b -> encode a <> b)
{-# INLINE encodeVector #-}

-- | Generic decoder for vectors. Its intended use is to allow easy
-- definition of 'Serialise' instances for custom vector
decodeVector :: (Bi a, Vector.Generic.Vector v a)
             => Decoder s (v a)
decodeVector = decodeContainerSkelWithReplicate
    decodeListLen
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

genericEncode :: (Generic a, GSerialiseEncode (Rep a)) => a -> Encoding
genericEncode = gencode . from

genericDecode :: (Generic a, GSerialiseDecode (Rep a)) => Decoder s a
genericDecode = to <$> gdecode

class GSerialiseEncode f where
    gencode  :: f a -> Encoding

class GSerialiseDecode f where
    gdecode  :: Decoder s (f a)

instance GSerialiseEncode V1 where
    -- Data types without constructors are still serialised as null value
    gencode _ = encodeNull

instance GSerialiseDecode V1 where
    gdecode   = error "V1 don't have contructors" <$ decodeNull

instance GSerialiseEncode U1 where
    -- Constructors without fields are serialised as null value
    gencode _ = encodeListLen 1 <> encodeWord 0

instance GSerialiseDecode U1 where
    gdecode   = do
      n <- decodeListLen
      when (n /= 1) $ fail "expect list of length 1"
      tag <- decodeWord
      when (tag /= 0) $ fail "unexpected tag. Expect 0"
      return U1

instance GSerialiseEncode a => GSerialiseEncode (M1 i c a) where
    -- Metadata (constructor name, etc) is skipped
    gencode = gencode . unM1

instance GSerialiseDecode a => GSerialiseDecode (M1 i c a) where
    gdecode = M1 <$> gdecode

instance Bi a => GSerialiseEncode (K1 i a) where
    -- Constructor field (Could only appear in one-field & one-constructor
    -- data types). In all other cases we go through GSerialise{Sum,Prod}
    gencode (K1 a) = encodeListLen 2
                  <> encodeWord 0
                  <> encode a

instance Bi a => GSerialiseDecode (K1 i a) where
    gdecode = do
      n <- decodeListLen
      when (n /= 2) $
        fail "expect list of length 2"
      tag <- decodeWord
      when (tag /= 0) $
        fail "unexpected tag. Expects 0"
      K1 <$> decode

instance (GSerialiseProd f, GSerialiseProd g) => GSerialiseEncode (f :*: g) where
    -- Products are serialised as N-tuples with 0 constructor tag
    gencode (f :*: g)
        = encodeListLen (nFields (Proxy :: Proxy (f :*: g)) + 1)
       <> encodeWord 0
       <> encodeSeq f
       <> encodeSeq g

instance (GSerialiseProd f, GSerialiseProd g) => GSerialiseDecode (f :*: g) where
    gdecode = do
      let nF = nFields (Proxy :: Proxy (f :*: g))
      n <- decodeListLen
      -- TODO FIXME: signedness of list length
      when (fromIntegral n /= nF + 1) $
        fail $ "Wrong number of fields: expected="++show (nF+1)++" got="++show n
      tag <- decodeWord
      when (tag /= 0) $
        fail $ "unexpect tag (expect 0)"
      !f <- gdecodeSeq
      !g <- gdecodeSeq
      return $ f :*: g

instance (GSerialiseSum f, GSerialiseSum g) => GSerialiseEncode (f :+: g) where
    -- Sum types are serialised as N-tuples and first element is
    -- constructor tag
    gencode a = encodeListLen (numOfFields a + 1)
             <> encode (conNumber a)
             <> encodeSum a

instance (GSerialiseSum f, GSerialiseSum g) => GSerialiseDecode (f :+: g) where
    gdecode = do
        n <- decodeListLen
        -- TODO FIXME: Again signedness
        when (n == 0) $
          fail "Empty list encountered for sum type"
        nCon  <- decodeWord
        trueN <- fieldsForCon (Proxy :: Proxy (f :+: g)) nCon
        when (n-1 /= fromIntegral trueN ) $
          fail $ "Number of fields mismatch: expected="++show trueN++" got="++show n
        decodeSum nCon


-- | Serialization of product types
class GSerialiseProd f where
    -- | Number of fields in product type
    nFields   :: Proxy f -> Word
    -- | Encode fields sequentially without writing header
    encodeSeq :: f a -> Encoding
    -- | Decode fields sequentially without reading header
    gdecodeSeq :: Decoder s (f a)

instance (GSerialiseProd f, GSerialiseProd g) => GSerialiseProd (f :*: g) where
    nFields _ = nFields (Proxy :: Proxy f) + nFields (Proxy :: Proxy g)
    encodeSeq (f :*: g) = encodeSeq f <> encodeSeq g
    gdecodeSeq = do !f <- gdecodeSeq
                    !g <- gdecodeSeq
                    return (f :*: g)

instance GSerialiseProd U1 where
    -- N.B. Could only be reached when one of constructors in sum type
    --      don't have parameters
    nFields   _ = 0
    encodeSeq _ = mempty
    gdecodeSeq  = return U1

instance (Bi a) => GSerialiseProd (K1 i a) where
    -- Ordinary field
    nFields    _     = 1
    encodeSeq (K1 f) = encode f
    gdecodeSeq       = K1 <$> decode

instance (i ~ S, GSerialiseProd f) => GSerialiseProd (M1 i c f) where
    -- We skip metadata
    nFields     _     = 1
    encodeSeq  (M1 f) = encodeSeq f
    gdecodeSeq        = M1 <$> gdecodeSeq

-- | Serialization of sum types
--
class GSerialiseSum f where
    -- | Number of constructor of given value
    conNumber   :: f a -> Word
    -- | Number of fields of given value
    numOfFields :: f a -> Word
    -- | Encode field
    encodeSum   :: f a  -> Encoding

    -- | Decode field
    decodeSum     :: Word -> Decoder s (f a)
    -- | Number of constructors
    nConstructors :: Proxy f -> Word
    -- | Number of fields for given constructor number
    fieldsForCon  :: Proxy f -> Word -> Decoder s Word

instance (GSerialiseSum f, GSerialiseSum g) => GSerialiseSum (f :+: g) where
    conNumber x = case x of
      L1 f -> conNumber f
      R1 g -> conNumber g + nConstructors (Proxy :: Proxy f)
    numOfFields x = case x of
      L1 f -> numOfFields f
      R1 g -> numOfFields g
    encodeSum x = case x of
      L1 f -> encodeSum f
      R1 g -> encodeSum g

    nConstructors _ = nConstructors (Proxy :: Proxy f)
                    + nConstructors (Proxy :: Proxy g)

    fieldsForCon _ n | n < nL    = fieldsForCon (Proxy :: Proxy f) n
                     | otherwise = fieldsForCon (Proxy :: Proxy g) (n - nL)
      where
        nL = nConstructors (Proxy :: Proxy f)

    decodeSum nCon | nCon < nL = L1 <$> decodeSum nCon
                   | otherwise = R1 <$> decodeSum (nCon - nL)
      where
        nL = nConstructors (Proxy :: Proxy f)

instance (i ~ C, GSerialiseProd f) => GSerialiseSum (M1 i c f) where
    conNumber    _     = 0
    numOfFields  _     = nFields (Proxy :: Proxy f)
    encodeSum   (M1 f) = encodeSeq f

    nConstructors  _ = 1
    fieldsForCon _ 0 = return $ nFields (Proxy :: Proxy f)
    fieldsForCon _ _ = fail "Bad constructor number"
    decodeSum      0 = M1 <$> gdecodeSeq
    decodeSum      _ = fail "bad constructor number"
