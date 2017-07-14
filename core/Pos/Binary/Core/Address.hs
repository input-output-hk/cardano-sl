-- | Binary serialization of Pos.Types.Address
module Pos.Binary.Core.Address () where

import           Universum

import           Data.Digest.CRC32   (CRC32 (..))
import           Pos.Binary.Crypto   ()
import           Pos.Core.Types      (Address)
import           Data.Default        (def)
import           Data.Word           (Word8)
import           Pos.Binary.Class    (Bi (..), serialize', deserialize', encodeListLen, enforceSize)
import           Pos.Binary.Crypto   ()
import           Pos.Core.Types      (AddrPkAttrs (..), Address (..))
import           Pos.Data.Attributes (Attributes, encodeAttributes, decodeAttributes)

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

----------------------------------------

instance Bi (Attributes AddrPkAttrs) where
    encode = encodeAttributes [(0, serialize' . addrPkDerivationPath)]
    decode = decodeAttributes def $ \n v acc -> case n of
        0 -> Just $ acc { addrPkDerivationPath = deserialize' v }
        _ -> Nothing

instance Bi Address where
    encode addr =
        encodeListLen 2 <> encodeAddr
      where
        encodeAddr = case addr of
            PubKeyAddress keyHash attrs ->
                   encode (0 :: Word8)
                <> encode (serialize' (keyHash, attrs))
            ScriptAddress scrHash ->
                   encode (1 :: Word8)
                <> encode (serialize' scrHash)
            RedeemAddress keyHash ->
                   encode (2 :: Word8)
                <> encode (serialize' keyHash)
            UnknownAddressType t bs ->
                   encode t
                <> encode bs

    decode = do
        enforceSize "Address" 2
        t  <- decode @Word8
        bs <- decode @ByteString
        pure $ case t of
            0 -> uncurry PubKeyAddress $ deserialize' bs
            1 -> ScriptAddress         $ deserialize' bs
            2 -> RedeemAddress         $ deserialize' bs
            _ -> UnknownAddressType t bs
