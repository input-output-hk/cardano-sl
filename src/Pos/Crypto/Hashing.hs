-- | Hashing capabilities.

module Pos.Crypto.Hashing
       ( Hash
       , hashConvert
       , hash
       ) where

import qualified Crypto.Hash as Hash (Digest, SHA256, hash, hashlazy)
import           Data.Binary (Binary)
import qualified Data.Binary as Binary
import           Universum

type Hash = Hash.Digest Hash.SHA256

hashConvert :: Binary a => a -> Hash
hashConvert = Hash.hashlazy . Binary.encode

hash :: ByteString -> Hash
hash = Hash.hash
