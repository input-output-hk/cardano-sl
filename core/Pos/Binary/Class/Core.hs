{-# LANGUAGE CPP #-}

#include "MachDeps.h"

-- | Bi typeclass and most basic functions.

module Pos.Binary.Class.Core
       ( Bi (..)
       , encode
       , decodeFull
       , getSize
       , biSize
       , label
       -- * Primitives for limiting serialization
       , limitGet
       , isolate64Full

       {-
       , encode
       , decode
       , decodeOrFail
       , decodeFull
       -}
       ) where

import           Universum

import           Control.Lens               (_Left)
import           Data.Store                 (PeekException (..), Size)
import           Data.Store.Core            (Peek (..), PeekResult (..), Poke (..))
import qualified Data.Store.Core            as Store
import qualified Data.Store.Internal        as Store
import           Foreign.Ptr                (minusPtr, plusPtr)
import           Serokell.Data.Memory.Units (Byte)

----------------------------------------------------------------------------
-- Bi typeclass
----------------------------------------------------------------------------

-- | Simplified definition of serializable object
-- Data.Binary.Class-alike.
--
-- Write @instance Bi SomeType where@ without any method definitions if you
-- want to use the 'Binary' instance for your type.
class Bi t where
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

getSize :: Bi a => a -> Int
getSize = Store.getSizeWith size
{-# INLINE getSize #-}

-- | Compute size of something serializable in bytes.
biSize :: Bi a => a -> Byte
biSize = fromIntegral . getSize
{-# INLINE biSize #-}

decodeFull :: Bi a => ByteString -> Either Text a
decodeFull = over _Left Store.peekExMessage . Store.decodeWith get

----------------------------------------------------------------------------
-- Basic functions for other modules
----------------------------------------------------------------------------

label :: Text -> Peek a -> Peek a
label msg p = Peek $ \pstate ptr ->
    runPeek p pstate ptr `catch` onPeekEx
  where
    onPeekEx (PeekException offset msgEx) =
        throwM (PeekException offset (msgEx <> "\n" <> msg))


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
