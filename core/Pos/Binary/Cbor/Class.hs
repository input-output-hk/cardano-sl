module Pos.Binary.Cbor.Class
    ( Bi(..)
    , encodeBinary
    , decodeBinary
    , enforceSize
    , matchSize
    ) where

import           Codec.CBOR.Decoding
import           Codec.CBOR.Encoding
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

instance Bi Int32 where
    encode = encodeInt32
    decode = decodeInt32

instance Bi Int64 where
    encode = encodeInt64
    decode = decodeInt64

instance Bi Nano where
    encode (MkFixed resolution) = encodeInteger resolution
    decode = MkFixed <$> decodeInteger

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

decodeMapSkel :: (Ord k, Bi k, Bi v) => ([(k, v)] -> m) -> Decoder s m
decodeMapSkel fromList = do
  n <- decodeMapLen
  fromList <$> decodeEntries n Nothing []
  where
    decodeEntries 0 _  !acc = pure acc
    decodeEntries i pk !acc = do
        !k <- decode
        !v <- decode
        whenJust pk $ \k' -> unless (k' < k) $
            fail "decodeMapSkel: keys are not encoded in strictly ascending order"
        decodeEntries (i - 1) (Just k) $ (k, v) : acc

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

decodeSetSkel :: (Ord a, Bi a) => ([a] -> c) -> Decoder s c
decodeSetSkel fromList = do
  n <- decodeListLen
  fromList <$> decodeEntries n Nothing []
  where
    decodeEntries 0 _  !acc = pure acc
    decodeEntries i pv !acc = do
        !v <- decode
        whenJust pv $ \v' -> unless (v' < v) $
            fail "decodeSetSkel: values are not encoded in strictly ascending order"
        decodeEntries (i - 1) (Just v) $ v : acc
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
