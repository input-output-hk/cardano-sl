-- | Binary serialization of 'Address' and related types.

module Pos.Binary.Core.Address () where

import           Universum
import           Unsafe                (unsafeFromJust)

import           Codec.CBOR.Encoding   (Encoding)
import qualified Codec.CBOR.Write      as CBOR.Write
import qualified Data.ByteString       as BS
import           Data.Digest.CRC32     (CRC32 (..))
import           Data.Word             (Word8)
import           Formatting            (build, formatToString, shown, (%))

import           Pos.Binary.Class      (Bi (..), decodeListLen, deserialize',
                                        encodeListLen, enforceSize, serialize')
import           Pos.Binary.Core.Types ()
import           Pos.Binary.Crypto     ()
import           Pos.Core.Types        (AddrAttributes (..), AddrSpendingData (..),
                                        AddrStakeDistribution (..), AddrType (..),
                                        Address (..), Address' (..))
import           Pos.Data.Attributes   (Attributes (..), decodeAttributes,
                                        encodeAttributes)

----------------------------------------------------------------------------
-- Helper types serialization
----------------------------------------------------------------------------

-- Helper function to avoid writing `:: Word8`.
w8 :: Word8 -> Word8
w8 = identity
{-# INLINE w8 #-}

instance Bi AddrType where
    encode =
        encode @Word8 . \case
            ATPubKey -> 0
            ATScript -> 1
            ATRedeem -> 2
            ATUnknown tag -> tag
    decode =
        decode @Word8 <&> \case
            0 -> ATPubKey
            1 -> ATScript
            2 -> ATRedeem
            tag -> ATUnknown tag

{- NOTE: Address spending data serialization
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

An address is serialized as a tuple consisting of:

1. One-byte tag.
2. Data dependent on tag.

If tag is 0, 1 or 2, the type of spending data is 'PubKeyASD',
'ScriptASD' or 'RedeemASD' respectively.

If tag is greater than 2, the data is decoded as a plain 'ByteString'.

This lets us have backwards compatibility. For instance, if a newer
version of CSL adds a new type of spending data with tag 3, then older
versions would deserialize it as follows:

    UnknownASD 3 <some bytes>
-}

instance Bi AddrSpendingData where
    encode =
        \case
            PubKeyASD pk -> encode (w8 0, pk)
            ScriptASD script -> encode (w8 1, script)
            RedeemASD redeemPK -> encode (w8 2, redeemPK)
            UnknownASD tag payload -> encode (tag, payload)
    decode = do
        enforceSize "AddrSpendingData" 2
        decode @Word8 >>= \case
            0 -> PubKeyASD <$> decode
            1 -> ScriptASD <$> decode
            2 -> RedeemASD <$> decode
            tag -> UnknownASD tag <$> decode

instance Bi AddrStakeDistribution where
    encode =
        \case
            BootstrapEraDistr -> encodeListLen 0
            SingleKeyDistr id -> encode (w8 0, id)
            MultiKeyDistr distr -> encode (w8 1, distr)
    decode =
        decodeListLen >>= \case
            0 -> pure BootstrapEraDistr
            2 ->
                decode @Word8 >>= \case
                    0 -> SingleKeyDistr <$> decode
                    1 -> MultiKeyDistr <$> decode
                    tag ->
                        fail $
                        "decode @AddrStakeDistribution: unexpected tag " <>
                        show tag
            len ->
                fail $
                "decode @AddrStakeDistribution: unexpected length " <> show len

{- NOTE: Address attributes serialization
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

'Attributes' are conceptually a map, where keys are numbers ('Word8').

For address there are two attributes:
• 0 — stake distribution, defaults to 'BootstrapEraDistr';
• 1 — derivation path, defaults to 'Nothing'.

-}

instance Bi (Attributes AddrAttributes) where
    encode attrs@(Attributes {attrData = AddrAttributes derivationPath stakeDistr}) =
        encodeAttributes listWithIndices attrs
      where
        listWithIndices :: [(Word8, AddrAttributes -> BS.ByteString)]
        listWithIndices =
            stakeDistributionListWithIndices <> derivationPathListWithIndices
        stakeDistributionListWithIndices =
            case stakeDistr of
                BootstrapEraDistr -> []
                _                 -> [(0, serialize' . aaStakeDistribution)]
        derivationPathListWithIndices =
            case derivationPath of
                Nothing -> []
                -- 'unsafeFromJust' is safe, because 'case' ensures
                -- that derivation path is 'Just'.
                Just _ ->
                    [(1, serialize' . unsafeFromJust . aaPkDerivationPath)]
    decode = decodeAttributes initValue go
      where
        initValue =
            AddrAttributes
            { aaPkDerivationPath = Nothing
            , aaStakeDistribution = BootstrapEraDistr
            }
        go n v acc =
            case n of
                0 -> pure acc {aaStakeDistribution = deserialize' v}
                1 -> pure acc {aaPkDerivationPath = Just $ deserialize' v}
                _ -> Nothing

-- We don't need a special encoding for 'Address'', GND is what we want.
deriving instance Bi Address'

----------------------------------------------------------------------------
-- Address serialization
----------------------------------------------------------------------------

{- NOTE: Address serialization
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

An address is serialized as a tuple consisting of:

1. 'addrRoot'.
2. 'addrAttributes'.
3. 'addrType'.
4. CRC32 checksum.
-}

instance CRC32 Address where
    crc32Update seed =
        crc32Update seed . CBOR.Write.toLazyByteString . encodeAddr

-- Encodes the `Address` __without__ the CRC32.
-- It's important to keep this function separated from the `encode`
-- definition to avoid that `encode` would call `crc32` and
-- the latter invoke `crc32Update`, which would then try to call `encode`
-- indirectly once again, in an infinite loop.
encodeAddr :: Address -> Encoding
encodeAddr Address {..} =
    encode addrRoot <> encode addrAttributes <> encode addrType

-- Note: we are using 'Buildable' constraint here, which in turn
-- relies on 'Bi', but it uses only encoding, while 'Buildable' is
-- used here only in decoding.
instance Buildable Address => Bi Address where
    encode addr = encodeListLen 4 <> encodeAddr addr <> encode (crc32 addr)
    decode = do
        (addrRoot, addrAttributes, addrType, decodedCRC) <- decode
        let res = Address {..}
        let actualCRC = crc32 res
        let errFmt = ("Address "%build%"has invalid checksum (decoded: "
                      %shown%", actual: "%shown%")")
        let err = formatToString errFmt res decodedCRC actualCRC
        res <$ unless (actualCRC == decodedCRC) (fail err)
