{-# LANGUAGE CPP                 #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

#include "MachDeps.h"

module Pos.Util.Binary
       (
         Raw

       -- * SafeCopy
       , getCopyBinary
       , putCopyBinary

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

       -- * Deserialisation with limited length
       , WithLengthLimited (..)

       -- * Other binary utils
       , getRemainingByteString
       , getAsciiString1b
       , putAsciiString1b
       ) where

import           Data.Binary.Get            (Get, getByteString,
                                            getRemainingLazyByteString, getWord8)
import           Data.Binary.Put            (Put, PutM, putByteString, putLazyByteString,
                                            putWord8, runPutM)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as BSL
import           Data.Char                  (isAscii)
import           Data.Reflection            (Reifies, reflect)
import           Data.SafeCopy              (Contained, SafeCopy (..), contain, safeGet,
                                            safePut)
import qualified Data.Serialize             as Cereal (Get, Put)
import           Formatting                 (formatToString, int, (%))
import           Node.Message               (Message (..))
import           Serokell.Data.Memory.Units (Byte)
import           Universum                  hiding (putByteString)

import           Pos.Binary.Class           (Bi (..))
import qualified Pos.Binary.Class           as Bi


-- | A wrapper over 'ByteString' for adding type safety to
-- 'Pos.Crypto.Pki.encryptRaw' and friends.
newtype Raw = Raw ByteString
    deriving (Bi, Eq, Ord, Show, Typeable)

----------------------------------------------------------------------------
-- SafeCopy
----------------------------------------------------------------------------

-- | A helper for "Data.SafeCopy" that creates 'putCopy' given a 'Binary'
-- instance.
putCopyBinary :: Bi a => a -> Contained Cereal.Put
putCopyBinary x = contain $ safePut (Bi.encode x)

-- | A helper for "Data.SafeCopy" that creates 'getCopy' given a 'Binary'
-- instance.
getCopyBinary :: Bi a => String -> Contained (Cereal.Get a)
getCopyBinary typeName = contain $ do
    bs <- safeGet
    case Bi.decodeFull bs of
        Left err -> fail ("getCopy@" ++ typeName ++ ": " ++ err)
        Right x  -> return x

----------------------------------------------------------------------------
-- Binary serialization
----------------------------------------------------------------------------

-- | See `Pos.Crypto.SerTypes` for details on this types
newtype AsBinary a = AsBinary
    { getAsBinary :: ByteString
    } deriving (Show, Eq, Ord, Hashable)

instance SafeCopy (AsBinary a) where
    getCopy = contain $ AsBinary <$> safeGet
    putCopy = contain . safePut . getAsBinary

class AsBinaryClass a where
  asBinary :: a -> AsBinary a
  fromBinary :: AsBinary a -> Either String a

fromBinaryM :: (AsBinaryClass a, MonadFail m) => AsBinary a -> m a
fromBinaryM = either fail return . fromBinary

----------------------------------------------------------------------------
-- Serialization with length
----------------------------------------------------------------------------

-- | Serialize something together with its length in bytes. The length comes
-- first.
putWithLength :: PutM a -> PutM a
putWithLength act = do
    let (res, serialized) = runPutM act
    let len :: Int64 = BSL.length serialized
    Bi.put (Bi.UnsignedVarInt len)
    putLazyByteString serialized
    return res

-- | Read length in bytes and then parse something (which has to have exactly
-- that length).
getWithLength :: Get a -> Get a
getWithLength act = do
    -- We limit the int to 20 bytes because an UnsignedVarInt Int64 takes at
    -- most 10 bytes. (20 and not 10 because it doesn't hurt to be cautious.)
    Bi.UnsignedVarInt (len :: Int64) <- Bi.limitGet 20 Bi.get
    Bi.isolate64 len act

-- | Read length in bytes, check that it's not bigger than the limit, and
-- then parse something (which has to have exactly parsed length).
getWithLengthLimited :: Int64 -> Get a -> Get a
getWithLengthLimited lim act = do
    Bi.UnsignedVarInt (len :: Int64) <- Bi.limitGet 20 Bi.get
    if len <= lim
        then Bi.isolate64 len act
        else fail $ formatToString
                      ("getWithLengthLimited: data ("%int%" bytes) is "%
                       "bigger than the limit ("%int%" bytes)")
                      len lim

-- | Like 'putWithLength', but should only be used for things that take less
-- than @2^14@ bytes.
--
-- Uses 'TinyVarInt' for storing length, thus guaranteeing that it won't take
-- more than 2 bytes and won't be ambiguous.
putSmallWithLength :: PutM a -> PutM a
putSmallWithLength act = do
    let (res, serialized) = runPutM act
    let len :: Int64 = BSL.length serialized
    if len >= 2^(14::Int)
        then panic ("putSmallWithLength: length is " <> show len <>
                    ", but maximum allowed is 16383 (2^14-1)")
        else do Bi.put (Bi.TinyVarInt (fromIntegral len))
                putLazyByteString serialized
                return res

-- | Like 'getWithLength' but for 'putSmallWithLength'.
getSmallWithLength :: Get a -> Get a
getSmallWithLength act = do
    Bi.TinyVarInt len <- Bi.get
    Bi.isolate64 (fromIntegral len) act

----------------------------------------------------------------------------
-- Deserialisation with limited length
----------------------------------------------------------------------------

-- | Sets size limit to deserialization instances via @s@ parameter
-- (using "Data.Reflection"). Grep for 'reify' and 'reflect' to see
-- usage examples.
newtype WithLengthLimited s a = WithLengthLimited
    { withLengthLimited :: a
    } deriving (Eq, Ord, Show)

instance (Bi a, Reifies s Byte) => Bi (WithLengthLimited s a) where
    put (WithLengthLimited a) = put a
    get = do
        let maxBlockSize = reflect (Proxy @s)
        getWithLengthLimited (fromIntegral maxBlockSize) $
            WithLengthLimited <$> get

deriving instance Message a => Message (WithLengthLimited s a)

----------------------------------------------------------------------------
-- Other binary utils
----------------------------------------------------------------------------

getRemainingByteString :: Get ByteString
getRemainingByteString = BSL.toStrict <$> getRemainingLazyByteString

getAsciiString1b :: String -> Word8 -> Get String
getAsciiString1b typeName limit = getWord8 >>= \sz -> do
            if sz > limit
               then fail $ typeName ++ " shouldn't be more than "
                                    ++ show limit ++ " bytes long"
               else traverse checkAscii =<< BS.unpack <$> getByteString (fromIntegral sz)
  where
    checkAscii (chr . fromIntegral -> c) =
        if isAscii c
           then return c
           else fail $ "Not an ascii symbol in " ++ typeName ++ " " ++ show c

putAsciiString1b :: String -> Put
putAsciiString1b str =  putWord8 (fromIntegral $ length str)
                     >> putByteString (BS.pack $ map (fromIntegral . ord) str)
