-- | Hashing capabilities.

module Pos.Crypto.Hashing
       ( Hash
       , hash
       , hashRaw
       , unsafeHash
       ) where

import qualified Crypto.Hash         as Hash (Digest, SHA256, hash, hashlazy)
import           Data.Binary         (Binary)
import qualified Data.Binary         as Binary
import qualified Data.Text.Buildable as Buildable
import           Formatting          (bprint, shown)
import           Universum

import           Pos.Util            (Raw)

newtype Hash a = Hash (Hash.Digest Hash.SHA256)
    deriving (Eq, Ord, Show)

instance Buildable.Buildable (Hash a) where
    build (Hash x) = bprint shown x

hash :: Binary a => a -> Hash a
hash = unsafeHash

hashRaw :: ByteString -> Hash Raw
hashRaw = Hash . Hash.hash

unsafeHash :: Binary a => a -> Hash b
unsafeHash = Hash . Hash.hashlazy . Binary.encode
