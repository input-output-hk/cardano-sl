-- | Hashing capabilities.

module Pos.Crypto.Hashing
       ( Hash
       , hashHexF
       , hash
       , hashRaw
       , unsafeHash
       ) where

import           Crypto.Hash         (Digest, SHA256, digestFromByteString)
import qualified Crypto.Hash         as Hash (hash, hashlazy)
import           Data.Binary         (Binary (..))
import qualified Data.Binary         as Binary
import qualified Data.Binary.Get     as Binary (getByteString)
import qualified Data.Binary.Put     as Binary (putByteString)
import qualified Data.ByteArray      as ByteArray
import qualified Data.Text.Buildable as Buildable
import           Formatting          (Format, bprint, later, shown)
import           Universum

import           Pos.Util            (Raw)

newtype Hash a = Hash (Digest SHA256)
    deriving (Show, Eq, Ord)

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
