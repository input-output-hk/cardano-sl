-- | Binary instances for transaction fee data.

module Pos.Binary.Core.Fee () where

import           Universum

import qualified Data.ByteString.Lazy as LBS
import           Data.Fixed (Nano)
import           Serokell.Data.Memory.Units (fromBytes)

import           Pos.Binary.Class (Bi (..), decode, decodeKnownCborDataItem,
                                   decodeUnknownCborDataItem, encode, encodeKnownCborDataItem,
                                   encodeListLen, encodeUnknownCborDataItem,
                                   encodedKnownCborDataItemSize, encodedUnknownCborDataItemSize,
                                   enforceSize)
import           Pos.Core.Common.Fee (Coeff (..), TxFeePolicy (..), TxSizeLinear (..))

instance Bi Coeff where
    encode (Coeff n) = encode n
    decode = Coeff <$> decode @Nano
    encodedSize (Coeff n) = encodedSize n

instance Bi TxSizeLinear where
    encode (TxSizeLinear a b) = encodeListLen 2 <> encode a <> encode b
    decode = do
        enforceSize "TxSizeLinear" 2
        !a <- decode @Coeff
        !b <- decode @Coeff
        return $ TxSizeLinear a b
    encodedSize (TxSizeLinear a b) = 1 + encodedSize a + encodedSize b

instance Bi TxFeePolicy where
    encode policy = case policy of
        TxFeePolicyTxSizeLinear txSizeLinear ->
            encodeListLen 2 <> encode (0 :: Word8)
                            <> encodeKnownCborDataItem txSizeLinear
        TxFeePolicyUnknown word8 bs          ->
            encodeListLen 2 <> encode word8
                            <> encodeUnknownCborDataItem (LBS.fromStrict bs)
    decode = do
        enforceSize "TxFeePolicy" 2
        tag <- decode @Word8
        case tag of
            0 -> TxFeePolicyTxSizeLinear <$> decodeKnownCborDataItem
            _ -> TxFeePolicyUnknown tag  <$> decodeUnknownCborDataItem

    encodedSize policy = case policy of
        TxFeePolicyTxSizeLinear txSizeLinear ->
            2 + encodedKnownCborDataItemSize txSizeLinear
        TxFeePolicyUnknown _ bs              ->
            let len = fromIntegral $ length bs
            in 2 + encodedUnknownCborDataItemSize (fromBytes len)
