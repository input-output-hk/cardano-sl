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
import           Data.Binary.Get      (ByteOffset, runGet, runGetOrFail)
import           Data.Binary.Put      (runPut)
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BSL
import           Universum
--import qualified Data.Binary as B

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

instance (Bi a, Bi b) => Bi (a, b) where
    {-# INLINE put #-}
    put (a, b) = put a <> put b
    {-# INLINE get #-}
    get = liftM2 (,) get get

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
