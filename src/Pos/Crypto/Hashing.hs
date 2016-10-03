-- | Hashing capabilities.

module Pos.Crypto.Hashing
       ( Hash
       , hashConvert
       , hash
       ) where

import qualified Crypto.Hash         as Hash (Digest, SHA256, hash, hashlazy)
import           Data.Binary         (Binary)
import qualified Data.Binary         as Binary
import qualified Data.Text.Buildable as Buildable
import           Formatting          (bprint, shown)
import           Universum

newtype Hash = Hash (Hash.Digest Hash.SHA256)
    deriving (Eq, Ord, Show)

instance Buildable.Buildable Hash where
    build (Hash x) = bprint shown x

hashConvert :: Binary a => a -> Hash
hashConvert = Hash . Hash.hashlazy . Binary.encode

hash :: ByteString -> Hash
hash = Hash . Hash.hash
