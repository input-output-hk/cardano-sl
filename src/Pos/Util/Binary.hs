{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}

#include "MachDeps.h"

module Pos.Util.Binary
       (
       -- * SafeCopy
         getCopyBinary
       , putCopyBinary
       , Raw

       -- * Binary serialization
       , AsBinary (..)
       , AsBinaryClass (..)
       , fromBinaryM

       -- * Serialization with length
       , putWithLength
       , getWithLength
       , getWithLengthLimited

       -- * Limiting serialization
       , limitGet
       , isolate64
       ) where

import           Formatting               ((%), formatToString, int)
import           Data.Binary.Get.Internal (Decoder (..), Get, runCont)
import           Data.Binary.Put          (PutM, putLazyByteString, runPutM)
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Lazy     as BSL
import           Data.SafeCopy            (Contained, SafeCopy (..), contain, safeGet,
                                           safePut)
import qualified Data.Serialize           as Cereal (Get, Put)
import           Universum
import           Unsafe.Coerce            (unsafeCoerce)

import           Pos.Binary.Class         (Bi)
import qualified Pos.Binary.Class         as Bi

-- | A wrapper over 'ByteString' for adding type safety to
-- 'Pos.Crypto.Pki.encryptRaw' and friends.
newtype Raw = Raw ByteString
    deriving (Bi, Eq, Ord, Show, Typeable)

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
        --
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
    Bi.UnsignedVarInt (len :: Int64) <- limitGet 20 Bi.get
    isolate64 len act

-- | Read length in bytes, check that it's not bigger than the limit, and
-- then parse something (which has to have exactly parsed length).
getWithLengthLimited :: Int64 -> Get a -> Get a
getWithLengthLimited lim act = do
    Bi.UnsignedVarInt (len :: Int64) <- limitGet 20 Bi.get
    if len <= lim
        then isolate64 len act
        else fail $ formatToString
                      ("getWithLengthLimited: data ("%int%" bytes) is "%
                       "bigger than the limit ("%int%" bytes)")
                      len lim

-- | Like 'isolate', but allows consuming less bytes than expected (just not
-- more).
limitGet :: Int64  -- ^ The upper limit on byte consumption
         -> Get a  -- ^ The decoder to isolate
         -> Get a
-- A modified version of 'isolate' from Data.Binary.Get
limitGet n0 act
  | n0 < 0 = fail "limitGet: negative size"
  | otherwise = go n0 (runCont act BS.empty Done)
  where
  go _ (Done left x) = pushFront left >> return x
  go 0 (Partial resume) = go 0 (resume Nothing)
  go n (Partial resume) = do
    inp <- unsafeCoerce (OurC (\inp k -> do
      let takeLimited str =
#if (WORD_SIZE_IN_BITS == 64)
            let (inp', out) = BS.splitAt (fromIntegral n) str
#else
            let (inp', out) = if n > fromIntegral (maxBound :: Int)
                                then (str, BS.empty)
                                else BS.splitAt (fromIntegral n) str
#endif
            in k out (Just inp')
      case not (BS.null inp) of
        True  -> takeLimited inp
        False -> prompt inp (k BS.empty Nothing) takeLimited))
    case inp of
      Nothing  -> go n (resume Nothing)
      Just str -> go (n - fromIntegral (length str)) (resume (Just str))
  go _ (Fail bs err) = pushFront bs >> fail err
  go n (BytesRead r resume) =
    go n (resume $! n0 - n - r)

-- | Like 'isolate', but works with Int64 only.
isolate64 :: Int64
          -> Get a
          -> Get a
isolate64 n0 act
  | n0 < 0 = fail "isolate64: negative size"
  | otherwise = go n0 (runCont act BS.empty Done)
  where
  go !n (Done left x)
    | n == 0 && BS.null left = return x
    | otherwise = do
        pushFront left
        let consumed = n0 - n - fromIntegral (BS.length left)
        fail $ "isolate: the decoder consumed " ++ show consumed ++ " bytes" ++
                 " which is less than the expected " ++ show n0 ++ " bytes"
  go 0 (Partial resume) = go 0 (resume Nothing)
  go n (Partial resume) = do
    inp <- unsafeCoerce (OurC (\inp k -> do
      let takeLimited str =
#if (WORD_SIZE_IN_BITS == 64)
            let (inp', out) = BS.splitAt (fromIntegral n) str
#else
            let (inp', out) = if n > fromIntegral (maxBound :: Int)
                                then (str, BS.empty)
                                else BS.splitAt (fromIntegral n) str
#endif
            in k out (Just inp')
      case not (BS.null inp) of
        True  -> takeLimited inp
        False -> prompt inp (k BS.empty Nothing) takeLimited))
    case inp of
      Nothing  -> go n (resume Nothing)
      Just str -> go (n - fromIntegral (BS.length str)) (resume (Just str))
  go _ (Fail bs err) = pushFront bs >> fail err
  go n (BytesRead r resume) =
    go n (resume $! n0 - n - r)

----------------------------------------------------------------------------
-- Guts of 'binary'
----------------------------------------------------------------------------

-- Using 'unsafeCoerce' here because 'C' isn't exported. Aargh. For now it'll
-- do and then I'll submit some pull requests to 'binary' and hopefully all
-- of this won't be needed. –@neongreen
pushFront :: ByteString -> Get ()
pushFront bs = unsafeCoerce (OurC (\ inp ks -> ks (BS.append bs inp) ()))
{-# INLINE pushFront #-}

-- This ***has*** to correspond to the implementation of 'Get' in 'binary'
-- because we're using it for 'unsafeCoerce'.
newtype OurGet a = OurC (forall r. ByteString ->
                                   OurSuccess a r ->
                                   Decoder      r )

-- Ditto.
type OurSuccess a r = ByteString -> a -> Decoder r

-- More functions from 'binary'.
prompt :: ByteString -> Decoder a -> (ByteString -> Decoder a) -> Decoder a
prompt inp kf ks = prompt' kf (\inp' -> ks (inp `BS.append` inp'))

-- And more.
prompt' :: Decoder a -> (ByteString -> Decoder a) -> Decoder a
prompt' kf ks =
  let loop =
        Partial $ \sm ->
          case sm of
            Just s | BS.null s -> loop
                   | otherwise -> ks s
            Nothing -> kf
  in loop
