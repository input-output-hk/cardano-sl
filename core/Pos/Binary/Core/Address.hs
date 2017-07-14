-- | Binary serialization of Pos.Types.Address
module Pos.Binary.Core.Address () where

import           Universum

import           Data.Digest.CRC32   (CRC32 (..))
import           Pos.Binary.Class    (Bi (..), serialize')
import           Pos.Binary.Crypto   ()
import           Pos.Core.Types      (Address)


{- NOTE: Address serialization
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

An address is serialized as follows:

    <one-byte tag><length of content><content>

This lets us have backwards compatibility. For instance, if a newer version
of CSL adds an address with tag 5:

    data Address
        = ...
        | SuperAddress A B C

then older versions would deserialize it as follows:

    UnknownAddressType 5 <some bytes>

The length is needed because otherwise we wouldn't know where the address
ends and the next part of the structure begins (if an address is embedded
into a bigger structure).

Moreover, since we want *any* address to be potentially deserializable as
UnknownAddressType, we add length to already existing addresses
(PubKeyAddress, ScriptAddress, etc) as well even though it's not strictly
necessary (because there are no previous versions of CSL to be
backwards-compatible with and so we could special-case those types of
addresses and save a byte).
-}

instance CRC32 Address where
    crc32Update seed = crc32Update seed . serialize'

-- Just a stub as @arybczak is working on it.
instance Bi Address where
  encode = error "Address encode: unimplemented"
  decode = fail  "Address decode: unimplemented"
