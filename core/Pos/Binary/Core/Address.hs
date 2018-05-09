-- | Binary serialization of 'Address' and related types.

module Pos.Binary.Core.Address (encodeAddr, encodeAddrCRC32) where

import           Universum

import           Codec.CBOR.Encoding (Encoding)

import           Pos.Binary.Class (Bi (..), encodeCrcProtected)
import           Pos.Core.Common.Types (Address (..))

----------------------------------------------------------------------------
-- Helper types serialization
----------------------------------------------------------------------------

-- Encodes the `Address` __without__ the CRC32.
-- It's important to keep this function separated from the `encode`
-- definition to avoid that `encode` would call `crc32` and
-- the latter invoke `crc32Update`, which would then try to call `encode`
-- indirectly once again, in an infinite loop.
encodeAddr :: Address -> Encoding
encodeAddr Address {..} =
    encode addrRoot <> encode addrAttributes <> encode addrType

encodeAddrCRC32 :: Address -> Encoding
encodeAddrCRC32 Address{..} = encodeCrcProtected (addrRoot, addrAttributes, addrType)
