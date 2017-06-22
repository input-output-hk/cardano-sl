{-# LANGUAGE ScopedTypeVariables #-}

-- | Useful functions for serialization/deserialization.

module Pos.Binary.Class.Primitive
       (
       -- * Primitives for serialization
         putWord8S
       , putBytesS

       -- * Bi to SafeCopy
       , getCopyBi
       , putCopyBi
       -- * Binary serialization
       , AsBinary (..)
       , AsBinaryClass (..)
       , fromBinaryM

       -- * Serialization with length
       , putWithLength
       , putWithLengthS
       , getWithLength
       , getWithLengthLimited
       , putSmallWithLength
       , putSmallWithLengthS
       , getSmallWithLength

       -- * Serialization strings and sequence of bytes
       , getBytes
       , putBytes

       , getAsciiString1b
       , putAsciiString1b
       , sizeAsciiString1b

       , Raw (..)
       ) where

import           Universum

import qualified Data.ByteString            as BS
import           Data.Char                  (isAscii)
import           Data.Reflection            (reifyNat)
import           Data.SafeCopy              (Contained, SafeCopy (..), contain, safeGet,
                                             safePut)
import qualified Data.Serialize             as Cereal (Get, Put)
import qualified Data.Store.Core            as Store
import qualified Data.Store.Internal        as Store
import           Formatting                 (formatToString, int, (%))

import           Pos.Binary.Class.Core      (Bi (..), decodeFull, encode, isolate64Full,
                                             limitGet)
import           Pos.Binary.Class.Numbers   (TinyVarInt (..), UnsignedVarInt (..),
                                             getWord8, putWord8)
import           Pos.Binary.Class.Store     (Peek, Poke, PokeWithSize (..), Size (..),
                                             StaticSize (..), putS)

import           Pos.Binary.Class.Instances ()
-- TODO ^ Move Raw in Pos.Util.Util and remove the import.

-- | See 'putWord8'.
putWord8S :: Word8 -> PokeWithSize ()
putWord8S = putS @Word8

-- | See 'putBytes'.
putBytesS :: ByteString -> PokeWithSize ()
putBytesS bs = PokeWithSize (BS.length bs) (putBytes bs)

----------------------------------------------------------------------------
-- Raw
----------------------------------------------------------------------------

-- | A wrapper over 'ByteString' for signalling that a bytestring should be
-- processed as a sequence of bytes, not as a separate “entity”. It's used in
-- crypto and binary code.
newtype Raw = Raw ByteString
    deriving (Bi, Eq, Ord, Show, Typeable, NFData)

{-
-- | 'getMany n' get 'n' elements in order, without blowing the stack.
getMany :: Bi a => Int -> Get [a]
getMany n = go [] n
 where
    go xs 0 = return $! reverse xs
    go xs i = do x <- get
                 -- we must seq x to avoid stack overflows due to laziness in
                 -- (>>=)
                 x `seq` go (x:xs) (i-1)
{-# INLINE getMany #-}
-}

----------------------------------------------------------------------------
-- SafeCopy
----------------------------------------------------------------------------

-- | A helper for "Data.SafeCopy" that creates 'putCopy' given a 'Bi'
-- instance.
putCopyBi :: Bi a => a -> Contained Cereal.Put
putCopyBi x = contain $ safePut (encode x)

-- | A helper for "Data.SafeCopy" that creates 'getCopy' given a 'Bi'
-- instance.
getCopyBi :: Bi a => Text -> Contained (Cereal.Get a)
getCopyBi typeName = contain $ do
    bs <- safeGet
    case decodeFull bs of
        Left err -> (fail . toString) ("getCopy@" <> typeName <> ": " <> err)
        Right x  -> return x

----------------------------------------------------------------------------
-- Binary serialization
----------------------------------------------------------------------------

newtype AsBinary a = AsBinary
    { getAsBinary :: ByteString
    } deriving (Show, Eq, Ord, Hashable, NFData)

instance SafeCopy (AsBinary a) where
    getCopy = contain $ AsBinary <$> safeGet
    putCopy = contain . safePut . getAsBinary

class AsBinaryClass a where
    asBinary :: a -> AsBinary a
    fromBinary :: AsBinary a -> Either Text a

fromBinaryM :: (AsBinaryClass a, MonadFail m) => AsBinary a -> m a
fromBinaryM = either (fail . toString) return . fromBinary

----------------------------------------------------------------------------
-- Serialization with length
----------------------------------------------------------------------------

-- | Serialize something together with its length in bytes. The length comes
-- first. If you want to serialize several things at once, use a tuple.
putWithLength :: PokeWithSize a -> Poke a
putWithLength a =
    put (UnsignedVarInt $ (fromIntegral (pwsToSize a) :: Int64)) *> pwsToPoke a

-- | Like @putWithLength@ but returns PokeWithSize.
putWithLengthS :: PokeWithSize a -> PokeWithSize a
putWithLengthS a =
    putS (UnsignedVarInt $ (fromIntegral (pwsToSize a) :: Int64)) *> a

-- | Read length in bytes and then parse something (which has to have exactly
-- that length).
getWithLength :: (Int64 -> Peek a) -> Peek a
getWithLength getter = do
    -- We limit the int to 20 bytes because an UnsignedVarInt Int64 takes at
    -- most 10 bytes. (20 and not 10 because it doesn't hurt to be cautious.)
    UnsignedVarInt (len :: Int64) <- limitGet 20 get
    isolate64Full len (getter len)

-- | Read length in bytes, check that it's not bigger than a specified limit,
-- and then parse something (which has to have exactly the parsed length).
getWithLengthLimited :: Int64 -> (Int64 -> Peek a) -> Peek a
getWithLengthLimited lim getter = do
    UnsignedVarInt (len :: Int64) <- limitGet 20 get
    if len <= lim
        then isolate64Full len (getter len)
        else fail $ formatToString
                      ("getWithLengthLimited: data ("%int%" bytes) is "%
                       "bigger than the limit ("%int%" bytes)")
                      len lim

-- | Like 'putWithLength', but should only be used for things that take less
-- than @2^14@ bytes.
--
-- Uses 'TinyVarInt' for storing length, thus guaranteeing that it won't take
-- more than 2 bytes and won't be ambiguous.
putSmallWithLength :: PokeWithSize a -> Poke a
putSmallWithLength = pwsToPoke . putSmallWithLengthS

-- | Like @putSmallWithLength@ but returns PokeWithSize.
putSmallWithLengthS :: PokeWithSize a -> PokeWithSize a
putSmallWithLengthS a@(PokeWithSize len _) =
    if len >= 2^(14::Int)
        then error ("putSmallWithLength: length is " <> show len <>
                    ", but maximum allowed is 16383 (2^14-1)")
        else putS (TinyVarInt (fromIntegral len)) *> a

-- | Like 'getWithLength' but for 'putSmallWithLength'.
getSmallWithLength :: (Word16 -> Peek a) -> Peek a
getSmallWithLength getter = do
    TinyVarInt len <- get
    isolate64Full (fromIntegral len) (getter len)

----------------------------------------------------------------------------
-- Other binary utils
----------------------------------------------------------------------------

-- [CSL-1122] create a newtype for this instead?
getAsciiString1b :: String -> Word8 -> Peek String
getAsciiString1b typeName limit = getWord8 >>= \sz -> do
            if sz > limit
               then fail $ typeName ++ " shouldn't be more than "
                                    ++ show limit ++ " bytes long"
               else traverse checkAscii =<< BS.unpack <$> getBytes (fromIntegral sz)
  where
    checkAscii (chr . fromIntegral -> c) =
        if isAscii c
           then return c
           else fail $ "Not an ascii symbol in " ++ typeName ++ " " ++ show c

putAsciiString1b :: String -> Poke ()
putAsciiString1b str =  putWord8 (fromIntegral $ length str)
                     >> putBytes (BS.pack $ map (fromIntegral . ord) str)

sizeAsciiString1b :: Size String
sizeAsciiString1b = VarSize $ \s -> 1 + length s

-- | Get some bytes from the input.
getBytes :: Int -> Peek ByteString
getBytes i =
    reifyNat (fromIntegral i) $
        \(_ :: Proxy n) -> unStaticSize <$>
        (Store.peek :: Peek (StaticSize n ByteString))

-- | Serialize some bytes.
--
-- (Unlike 'put' for 'ByteString', it doesn't write the number of bytes into
-- the stream, only the bytes themselves.)
putBytes :: ByteString -> Poke ()
putBytes bs =
    reifyNat (fromIntegral $ BS.length bs) $ \(_ :: Proxy n) ->
        Store.poke (Store.toStaticSizeEx bs :: StaticSize n ByteString)
