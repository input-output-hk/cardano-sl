module Pos.Core.Common.AddressHash
       ( AddressHash
       , addressHash
       , unsafeAddressHash
       ) where

import           Universum

import           Crypto.Hash (Blake2b_224, Digest, SHA3_256)
import qualified Crypto.Hash as CryptoHash

import           Pos.Binary.Class (Bi)
import qualified Pos.Binary.Class as Bi
import           Pos.Crypto.Hashing (AbstractHash (..))

-- | Hash used to identify address.
type AddressHash = AbstractHash Blake2b_224

unsafeAddressHash :: Bi a => a -> AddressHash b
unsafeAddressHash = AbstractHash . secondHash . firstHash
  where
    firstHash :: Bi a => a -> Digest SHA3_256
    firstHash = CryptoHash.hashlazy . Bi.serialize
    secondHash :: Digest SHA3_256 -> Digest Blake2b_224
    secondHash = CryptoHash.hash

addressHash :: Bi a => a -> AddressHash a
addressHash = unsafeAddressHash
