{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Hashing capabilities.

module Pos.Crypto.Hashing
       ( Hash
       , hashHexF
       , shortHashF
       , hash
       , hashRaw
       , unsafeHash

       , CastHash (castHash)
       ) where

import           Crypto.Hash         (Digest, SHA256, digestFromByteString)
import qualified Crypto.Hash         as Hash (hash, hashlazy)
import           Data.Binary         (Binary (..))
import qualified Data.Binary         as Binary
import qualified Data.Binary.Get     as Binary (getByteString)
import qualified Data.Binary.Put     as Binary (putByteString)
import qualified Data.ByteArray      as ByteArray
import           Data.Hashable       (Hashable (hashWithSalt))
import           Data.MessagePack    (MessagePack (fromObject, toObject), Object (..))
import           Data.SafeCopy       (SafeCopy (..))
import qualified Data.Text.Buildable as Buildable
import           Formatting          (Format, bprint, fitLeft, later, shown, (%.))
import           Universum

import           Pos.Util            (Raw, getCopyBinary, msgpackFail, putCopyBinary)

-- | Hash wrapper with phantom type for more type-safety.
newtype Hash a = Hash (Digest SHA256)
    deriving (Show, Eq, Ord, ByteArray.ByteArrayAccess, Generic, NFData)

instance Hashable (Hash a) where
    hashWithSalt s (Hash x) = hashWithSalt s $ ByteArray.unpack x

instance MessagePack (Hash a) where
    toObject (Hash x) = toObject @ByteString . ByteArray.convert $ x
    fromObject (ObjectBin bs) =
        case digestFromByteString bs of
            Nothing -> msgpackFail "failed to convert ByteString to Hash"
            Just x  -> pure $ Hash x
    fromObject _ = msgpackFail "Hash must be represented as binary"

instance SafeCopy (Hash a) where
    putCopy = putCopyBinary
    getCopy = getCopyBinary "Hash"

instance Binary (Hash a) where
    get = do
        bs <- Binary.getByteString (256 `div` 8)
        case digestFromByteString bs of
            -- It's impossible because getByteString will already fail if
            -- there weren't enough bytes available
            Nothing -> panic "Pos.Crypto.Hashing.get: impossible"
            Just x  -> return (Hash x)
    put (Hash h) =
        Binary.putByteString (ByteArray.convert h)

instance Buildable.Buildable (Hash a) where
    build (Hash x) = bprint shown x

hash :: Binary a => a -> Hash a
hash = unsafeHash

hashRaw :: ByteString -> Hash Raw
hashRaw = Hash . Hash.hash

unsafeHash :: Binary a => a -> Hash b
unsafeHash = Hash . Hash.hashlazy . Binary.encode

hashHexF :: Format r (Hash a -> r)
hashHexF = later $ \(Hash x) -> Buildable.build (show x :: Text)

shortHashF :: Format r (Hash a -> r)
shortHashF = fitLeft 8 %. hashHexF

-- | Type class for unsafe cast between hashes.
-- You must ensure that types have identical Binary instances.
class CastHash a b where
    castHash :: Hash a -> Hash b
    castHash (Hash x) = Hash x

instance CastHash a a where
    castHash = identity
