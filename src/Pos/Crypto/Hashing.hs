-- | Hashing capabilities.

module Pos.Crypto.Hashing
       ( Hash
       , hashHexF
       , hash
       , hashRaw
       , unsafeHash
       ) where

import qualified Crypto.Hash         as Hash (Digest, SHA256, hash, hashlazy)
import           Data.Binary         (Binary (..))
import qualified Data.Binary         as Binary
import qualified Data.Text.Buildable as Buildable
import           Formatting          (Format, bprint, later, shown)
import           Universum

import           Pos.Util            (Raw)

newtype Hash a = Hash (Hash.Digest Hash.SHA256)
    deriving (Show, Eq, Ord)

instance Binary (Hash a) where
    -- TODO (cc @neongreen)
    get = undefined
    put = undefined

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
