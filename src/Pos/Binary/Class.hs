{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
-- | Serialization-related types

module Pos.Binary.Class
       ( Bi (..)
       , encode
       , decode
       , decodeOrFail
       , decodeFull

       , Serialized (..)
       , deserializeM
       ) where

import           Control.Monad.Fail   (MonadFail, fail)
import           Data.Binary          (Get, Put)
import qualified Data.Binary          as Binary
import           Data.Binary.Get      (ByteOffset, getWord8, runGet, runGetOrFail)
import           Data.Binary.Put      (putCharUtf8, putWord8, runPut)
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Hashable        (Hashable (..))
import qualified Data.HashMap.Strict  as HM
import qualified Data.List.NonEmpty   as NE
import           Data.Word            (Word32)
import           Universum

----------------------------------------------------------------------------
-- Bi typeclass
----------------------------------------------------------------------------

-- | Simplified definition of serializable object,
-- Data.Binary.Class-alike.
class Bi t where
    put :: t -> Put
    get :: Get t

--instance Serializable t => B.Binary t where
--    get = get
--    put = put

-- | Encode a value to a strict bytestring
encode :: Bi a => a -> BSL.ByteString
encode = runPut . put
{-# INLINE encode #-}

-- | Decode a value from a lazy ByteString, reconstructing the original structure.
decode :: Bi a => BSL.ByteString -> a
decode = runGet get

decodeOrFail
    :: Bi a
    => BSL.ByteString
    -> Either (BSL.ByteString, ByteOffset, [Char])
              (BSL.ByteString, ByteOffset, a)
decodeOrFail = runGetOrFail get

-- | Like 'decode', but ensures that the whole input has been consumed.
decodeFull :: Bi a => BSL.ByteString -> Either [Char] a
decodeFull bs = case (runGetOrFail get) bs of
    Left (_, _, err) -> Left ("decodeFull: " ++ err)
    Right (unconsumed, _, a)
        | BSL.null unconsumed -> Right a
        | otherwise -> Left "decodeFull: unconsumed input"

----------------------------------------------------------------------------
-- Popular basic instances
----------------------------------------------------------------------------

-- TODO get rid of boilerplate (or rewrite by hands to make it more clear)
-- I just copied most of it from here:
-- https://hackage.haskell.org/package/binary-0.8.4.1/docs/src/Data.Binary.Class.html#line-564

{-
Copyright (c) Lennart Kolmodin

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:

1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.

3. Neither the name of the author nor the names of his contributors
   may be used to endorse or promote products derived from this software
   without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE CONTRIBUTORS ``AS IS'' AND ANY EXPRESS
OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.
-}

instance Bi () where
    put ()  = mempty
    get     = return ()

instance Bi Bool where
    put     = putWord8 . fromIntegral . fromEnum
    get     = getWord8 >>= toBool
      where
        toBool 0 = return False
        toBool 1 = return True
        toBool c = fail ("Could not map value " ++ show c ++ " to Bool")

-- Words8s are written as bytes
instance Bi Word8 where
    put     = putWord8
    get     = getWord8

instance Bi Char where
    {-# INLINE put #-}
    put = putCharUtf8
    get = do
        let getByte = liftM (fromIntegral :: Word8 -> Int) get
            shiftL6 = flip shiftL 6 :: Int -> Int
        w <- getByte
        r <- case () of
                _ | w < 0x80  -> return w
                  | w < 0xe0  -> do
                                    x <- liftM (xor 0x80) getByte
                                    return (x .|. shiftL6 (xor 0xc0 w))
                  | w < 0xf0  -> do
                                    x <- liftM (xor 0x80) getByte
                                    y <- liftM (xor 0x80) getByte
                                    return (y .|. shiftL6 (x .|. shiftL6
                                            (xor 0xe0 w)))
                  | otherwise -> do
                                x <- liftM (xor 0x80) getByte
                                y <- liftM (xor 0x80) getByte
                                z <- liftM (xor 0x80) getByte
                                return (z .|. shiftL6 (y .|. shiftL6
                                        (x .|. shiftL6 (xor 0xf0 w))))
        getChr r
      where
        getChr w
          | w <= 0x10ffff = return $! toEnum $ fromEnum w
          | otherwise = fail "Not a valid Unicode code point!"

instance Bi BS.ByteString where
    {-# INLINE put #-}
    put = Binary.put
    {-# INLINE get #-}
    get = Binary.get

instance Bi BSL.ByteString where
    {-# INLINE put #-}
    put = Binary.put
    {-# INLINE get #-}
    get = Binary.get

instance Bi Int64 where
    {-# INLINE put #-}
    put = Binary.put
    {-# INLINE get #-}
    get = Binary.get

instance Bi Int where
    {-# INLINE put #-}
    put = Binary.put
    {-# INLINE get #-}
    get = Binary.get

instance Bi Integer where
    {-# INLINE put #-}
    put = Binary.put
    {-# INLINE get #-}
    get = Binary.get

instance Bi Word32 where
    {-# INLINE put #-}
    put = Binary.put
    {-# INLINE get #-}
    get = Binary.get

instance (Bi a, Bi b) => Bi (a, b) where
    {-# INLINE put #-}
    put (a, b) = put a <> put b
    {-# INLINE get #-}
    get = liftM2 (,) get get

instance (Bi a, Bi b, Bi c) => Bi (a, b, c) where
    {-# INLINE put #-}
    put (a, b, c) = put a <> put b <> put c
    {-# INLINE get #-}
    get = liftM3 (,,) get get get

instance (Bi a, Bi b, Bi c, Bi d) => Bi (a, b, c, d) where
    {-# INLINE put #-}
    put (a, b, c, d) = put a <> put b <> put c <> put d
    {-# INLINE get #-}
    get = liftM4 (,,,) get get get get


-- TODO Optimize by using varint instead of int
instance Bi a => Bi [a] where
    put xs = put (length xs) <> mapM_ put xs
    get = do n <- get :: Get Int
             getMany n

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

instance (Bi a, Bi b) => Bi (Either a b) where
    put (Left  a) = putWord8 0 <> put a
    put (Right b) = putWord8 1 <> put b
    get = do
        w <- getWord8
        case w of
            0 -> liftM Left  get
            _ -> liftM Right get

instance Bi a => Bi (NE.NonEmpty a) where
    get = fmap NE.fromList get
    put = put . NE.toList

instance (Bi a) => Bi (Maybe a) where
    put Nothing  = putWord8 0
    put (Just x) = putWord8 1 <> put x
    get = do
        w <- getWord8
        case w of
            0 -> return Nothing
            _ -> liftM Just get

instance  (Hashable k, Eq k, Bi k, Bi v) => Bi (HM.HashMap k v) where
    get = fmap HM.fromList get
    put = put . HM.toList

instance Bi Text where
    put = Binary.put
    get = Binary.get

----------------------------------------------------------------------------
-- Deserialized wrapper
----------------------------------------------------------------------------

class Bi b => Serialized a b where
    serialize :: a -> b
    deserialize :: b -> Either [Char] a

deserializeM :: (Serialized a b, MonadFail m) => b -> m a
deserializeM = either fail return . deserialize

instance (Serialized a c, Serialized b d) => Serialized (a, b) (c, d) where
    serialize (a, b) = (serialize a, serialize b)
    deserialize (c, d) = (,) <$> deserialize c <*> deserialize d
