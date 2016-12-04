{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Serialization-related types

module Pos.Binary.Class
       ( Bi (..)
       , encode
       , decode
       , decodeOrFail
       , decodeFull
       ) where

import           Data.Binary          (Get, Put)
import           Data.Binary.Get      (ByteOffset, runGet, runGetOrFail)
import           Data.Binary.Put      (runPut)
import qualified Data.ByteString.Lazy as BSL
import           Universum
--import qualified Data.Binary as B

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
