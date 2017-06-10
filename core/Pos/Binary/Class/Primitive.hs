{-# LANGUAGE ScopedTypeVariables #-}

module Pos.Binary.Class.Primitive
       (
       -- * Primitives for serialization
       putWord8WithSize

       -- * Bi to SafeCopy
       , getCopyBi
       , putCopyBi
       -- * Binary serialization
       , AsBinary (..)
       , AsBinaryClass (..)
       , fromBinaryM

       -- * Serialization with length
       , putWithLength
       , getWithLength
       , getWithLengthLimited
       , putSmallWithLength
       , getSmallWithLength

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
import qualified Data.Text                  as T
import           Formatting                 (formatToString, int, (%))

import           Pos.Binary.Class.Core      (Bi (..), decodeFull, encode, isolate64Full,
                                             limitGet)
import           Pos.Binary.Class.Numbers   (TinyVarInt (..), UnsignedVarInt (..),
                                             getWord8, putWord8)
import           Pos.Binary.Class.Store     (Peek, Poke, PokeWithSize (..), Size (..),
                                             StaticSize (..), pokeWithSize)

import           Pos.Binary.Class.Instances ()
-- TODO ^ Move Raw in Pos.Crypto and remove the import.

putWord8WithSize :: Word8 -> PokeWithSize ()
putWord8WithSize = pokeWithSize @Word8

----------------------------------------------------------------------------
-- Raw
----------------------------------------------------------------------------

-- | A wrapper over 'ByteString' for adding type safety to
-- 'Pos.Crypto.Pki.encryptRaw' and friends.
--
-- TODO: maybe it should be in "Pos.Crypto"?
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
        Left err -> (fail . T.unpack) ("getCopy@" <> typeName <> ": " <> err)
        Right x  -> return x

----------------------------------------------------------------------------
-- Binary serialization
----------------------------------------------------------------------------

-- | See `Pos.Crypto.SerTypes` for details on this types
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
fromBinaryM = either (fail . T.unpack) return . fromBinary

----------------------------------------------------------------------------
-- Serialization with length
----------------------------------------------------------------------------

-- | Serialize something together with its length in bytes. The length comes
-- first. If you want to serialize several things at once, use a tuple.
putWithLength :: PokeWithSize a -> Poke a
putWithLength a = put (UnsignedVarInt $ pwsToSize a) *> pwsToPoke a

-- | Read length in bytes and then parse something (which has to have exactly
-- that length).
getWithLength :: Peek a -> Peek a
getWithLength getter = do
    -- We limit the int to 20 bytes because an UnsignedVarInt Int64 takes at
    -- most 10 bytes. (20 and not 10 because it doesn't hurt to be cautious.)
    UnsignedVarInt (len :: Int64) <- limitGet 20 get
    isolate64Full len getter

-- | Read length in bytes, check that it's not bigger than a specified limit,
-- and then parse something (which has to have exactly the parsed length).
getWithLengthLimited :: Int64 -> Peek a -> Peek a
getWithLengthLimited lim getter = do
    UnsignedVarInt (len :: Int64) <- limitGet 20 get
    if len <= lim
        then isolate64Full len getter
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
putSmallWithLength (PokeWithSize pk len) = do
    if len >= 2^(14::Int)
        then error ("putSmallWithLength: length is " <> show len <>
                    ", but maximum allowed is 16383 (2^14-1)")
        else put (TinyVarInt (fromIntegral len)) *> pk

-- | Like 'getWithLength' but for 'putSmallWithLength'.
getSmallWithLength :: Peek a -> Peek a
getSmallWithLength getter = do
    TinyVarInt len <- get
    isolate64Full (fromIntegral len) getter

{-
----------------------------------------------------------------------------
-- Other binary utils
----------------------------------------------------------------------------

getRemainingByteString :: Peek ByteString
getRemainingByteString = BSL.toStrict <$> getRemainingLazyByteString
-}

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

-- | Get bytestring with constant length.
getBytes :: Int -> Peek ByteString
getBytes i =
    reifyNat (fromIntegral i) $
        \(_ :: Proxy n) -> unStaticSize <$>
        (Store.peek :: Peek (StaticSize n ByteString))

-- | Put bytestring with constant length.
putBytes :: ByteString -> Poke ()
putBytes bs =
    reifyNat (fromIntegral $ BS.length bs) $ \(_ :: Proxy n) ->
        Store.poke (Store.toStaticSizeEx bs :: StaticSize n ByteString)

-- putByteStringWithSize :: ByteString -> PokeWithSize ()
-- putByteStringWithSize bs =
--     reifyNat (fromIntegral $ BS.length bs) $ \(_ :: Proxy n) ->
--         PokeWithSize
--             (Store.poke (Store.toStaticSizeEx bs :: StaticSize n ByteString))
--             (fromIntegral $ BS.length bs)
