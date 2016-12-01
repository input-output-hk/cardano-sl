-- | Special hash functions for address building.

module Pos.Crypto.Address
       ( AddressHash
       , addressHash
       ) where

import           Crypto.Hash        (Blake2s_224, Digest, SHA3_256, hash, hashlazy)
import           Data.Binary        (Binary)
import qualified Data.Binary        as Binary
import           Universum

import           Pos.Crypto.Hashing (AbstractHash (..))

type AddressHash = AbstractHash Blake2s_224

firstHash :: Binary a => a -> Digest SHA3_256
firstHash = hashlazy . Binary.encode

secondHash :: Digest SHA3_256 -> Digest Blake2s_224
secondHash = hash

addressHash :: Binary a => a -> AddressHash a
addressHash = AbstractHash . secondHash . firstHash
