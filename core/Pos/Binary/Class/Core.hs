{-# LANGUAGE CPP #-}

#include "MachDeps.h"

-- | Bi typeclass and most basic functions.

module Pos.Binary.Class.Core
       ( Bi (..)
       , encode
       , encodeWithS
       , encodeLazy
       , decodeFull
       , decodeOrFail
       , getSize
       , biSize
       , label
       , labelP
       , labelS
       -- * Primitives for limiting serialization
       , limitGet
       , isolate64Full
       ) where

import           Universum

import           Control.Lens               (_Left)
import qualified Data.ByteString.Lazy       as BSL
import           Data.Store                 (Size)
import           Data.Store.Core            (Peek (..), PeekResult (..), Poke (..))
import qualified Data.Store.Core            as Store
import           Data.Store.Internal        (PeekException (..), PokeException (..))
import qualified Data.Store.Internal        as Store
import           Foreign.Ptr                (minusPtr, plusPtr)
import           Formatting                 (build, sformat, shown, (%))
import           Serokell.Data.Memory.Units (Byte)

----------------------------------------------------------------------------
-- Bi typeclass
----------------------------------------------------------------------------

-- | Simplified definition of serializable object
-- Data.Binary.Class-alike.
--
-- You can implement @put@ and @size@ or only @sizeNPut@.
-- @sizeNPut@ is needed for convenient way to implement @put@ and @size@
-- together without boilerplate code. It also makes instance less error-prone.
-- Please implement @sizeNPut@ instead of @size@ and @put@ if it's possible.
-- There are some useful helpers at Pos.Binary.Class.Store (like @putField@, @putConst@, etc)
class Bi t where
    {-# MINIMAL get, put, size | get, sizeNPut  #-}
    sizeNPut :: (Size t, t -> Poke ())
    sizeNPut = (size, put)

    size :: Size t
    size = fst sizeNPut

    put :: t -> Poke ()
    put = snd sizeNPut

    get :: Peek t


--instance Serializable t => B.Binary t where
--    get = get
--    put = put

-- | Encode a value to a strict bytestring
encode :: Bi a => a -> ByteString
encode x = Store.unsafeEncodeWith (put x) (getSize x)
{-# INLINE encode #-}

encodeWithS :: (Size a, a -> Poke ()) -> a -> ByteString
encodeWithS (s, p) x = Store.unsafeEncodeWith (p x) (Store.getSizeWith s x)
{-# INLINE encodeWithS #-}

-- | Encode a value to a lazy bytestring
encodeLazy :: Bi a => a -> BSL.ByteString
encodeLazy x = BSL.fromStrict (Store.unsafeEncodeWith (put x) (getSize x))
{-# INLINE encodeLazy #-}

getSize :: Bi a => a -> Int
getSize = Store.getSizeWith size
{-# INLINE getSize #-}

-- | Compute size of something serializable in bytes.
biSize :: Bi a => a -> Byte
biSize = fromIntegral . getSize
{-# INLINE biSize #-}

-- | Try to encode a strict ByteString,
-- return text of a thrown exception, if it failed.
decodeFull :: Bi a => ByteString -> Either Text a
decodeFull = over _Left Store.peekExMessage . Store.decodeWith get

-- | Try to encode a strict ByteString, call @error@ if it failed.
decodeOrFail :: Bi a => ByteString -> a
decodeOrFail a =
    (either (error . sformat ("Couldn't decode: "%shown%", reason: "%build) a) identity) $
    decodeFull a

----------------------------------------------------------------------------
-- Basic functions for other modules
----------------------------------------------------------------------------

-- | Append to exception message passed text, if decoding failed.
label :: Text -> Peek a -> Peek a
label msg p = Peek $ \pstate ptr ->
    runPeek p pstate ptr `catch` onPeekEx
  where
    onPeekEx (PeekException offset msgEx) =
        throwM (PeekException offset (msgEx <> "\n" <> msg))

labelP :: Text -> Poke a -> Poke a
labelP msg p = Poke $ \pstate off ->
    runPoke p pstate off `catch` onPokeEx
  where
    onPokeEx (PokeException offset msgEx) =
        throwM (PokeException offset (msgEx <> "\n" <> msg))

labelS :: Text -> (Size t, t -> Poke ()) -> (Size t, t -> Poke ())
labelS msg (s, poke) = (s, labelP msg . poke)

-- | Like 'isolate', but allows consuming less bytes than expected (just not
-- more).
-- Differences from `Store.isolate`:
--  * safely handles `Int64` length argument
--  * advances pointer only by bytes read
{-# INLINE limitGet #-}
limitGet :: Int64 -> Peek a -> Peek a
limitGet len m = Peek $ \ps ptr -> do
    let end = Store.peekStateEndPtr ps
        remaining = end `minusPtr` ptr
        len' = fromIntegral $ min (fromIntegral remaining) len
        ptr2 = ptr `plusPtr` len'
    PeekResult ptr' x <- Store.runPeek m ps ptr
    when (ptr' > ptr2) $
        throwM $ PeekException (ptr' `minusPtr` ptr2) "Overshot end of isolated bytes"
    return $ PeekResult ptr' x

-- | Isolate the input to n bytes, skipping n bytes forward. Fails if @m@
-- advances the offset beyond the isolated region.
-- Differences from `Store.isolate`:
--  * safely handles `Int64` length argument
--  * requires isolated input to be fully consumed
{-# INLINE isolate64Full #-}
isolate64Full :: Int64 -> Peek a -> Peek a
isolate64Full len m = Peek $ \ps ptr -> do
    let end = Store.peekStateEndPtr ps
        remaining = end `minusPtr` ptr
    when (len > fromIntegral remaining) $
      -- Do not perform the check on the new pointer, since it could have overflowed
#if (WORD_SIZE_IN_BITS >= 64)
      Store.tooManyBytes (fromIntegral len) remaining "isolate64"
#else
      (if len <= (maxBound :: Int)
         then Store.tooManyBytes (fromIntegral len) remaining "isolate64"
         else throwM $ PeekException 0 "")
#endif
    PeekResult ptr' x <- Store.runPeek m ps ptr
    let ptr2 = ptr `plusPtr` fromIntegral len
    when (ptr' < ptr2) $
        throwM $ PeekException (ptr2 `minusPtr` ptr') "Not all isolated bytes read"
    when (ptr' > ptr2) $
        throwM $ PeekException (ptr' `minusPtr` ptr2) "Overshot end of isolated bytes"
    return $ PeekResult ptr2 x
