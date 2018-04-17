{-# LANGUAGE BinaryLiterals #-}

-- | Communication-related serialization -- messages mostly.

module Pos.Binary.Communication
    (
    ) where

import           Universum

import qualified Data.ByteString.Lazy as LBS
import           Serokell.Data.Memory.Units (fromBytes)

import           Pos.Binary.Class (Bi (..), Cons (..), Field (..), decodeKnownCborDataItem,
                                   decodeUnknownCborDataItem, deriveSimpleBi,
                                   encodeKnownCborDataItem, encodedKnownCborDataItemSize,
                                   encodeListLen, encodeUnknownCborDataItem,
                                   encodedUnknownCborDataItemSize, enforceSize, withSize)
import           Pos.Binary.Core ()
import           Pos.Block.BHelpers ()
import           Pos.Block.Network (MsgBlock (..), MsgGetBlocks (..), MsgGetHeaders (..),
                                    MsgHeaders (..))
import           Pos.Communication.Types.Protocol (HandlerSpec (..), HandlerSpecs,
                                                   MsgSubscribe (..), MsgSubscribe1 (..),
                                                   VerInfo (..))
import           Pos.Core (BlockVersion, HeaderHash)
import           Pos.Util.Util (cborError)

-- TODO: move into each component

----------------------------------------------------------------------------
-- Blocks
----------------------------------------------------------------------------

deriveSimpleBi ''MsgGetHeaders [
    Cons 'MsgGetHeaders [
        Field [| mghFrom :: [HeaderHash]     |],
        Field [| mghTo   :: Maybe HeaderHash |]
    ]]

deriveSimpleBi ''MsgGetBlocks [
    Cons 'MsgGetBlocks [
        Field [| mgbFrom :: HeaderHash |],
        Field [| mgbTo   :: HeaderHash |]
    ]]

instance Bi MsgHeaders where
    encode = \case
        (MsgHeaders b) -> encodeListLen 2 <> encode (0 :: Word8) <> encode b
        (MsgNoHeaders t) -> encodeListLen 2 <> encode (1 :: Word8) <> encode t
    encodedSize = \case
        (MsgHeaders b) -> 2 + encodedSize b
        (MsgNoHeaders t) -> 2 + encodedSize t
    decode = do
        enforceSize "MsgHeaders" 2
        tag <- decode @Word8
        case tag of
            0 -> MsgHeaders <$> decode
            1 -> MsgNoHeaders <$> decode
            t -> cborError $ "MsgHeaders wrong tag: " <> show t

instance Bi MsgBlock where
    encode = \case
        (MsgBlock b) -> encodeListLen 2 <> encode (0 :: Word8) <> encode b
        (MsgNoBlock t) -> encodeListLen 2 <> encode (1 :: Word8) <> encode t
    encodedSize = \case
        (MsgBlock b) -> 2 + encodedSize b
        (MsgNoBlock t) -> 2 + encodedSize t
    decode = do
        enforceSize "MsgBlock" 2
        tag <- decode @Word8
        case tag of
            0 -> MsgBlock <$> decode
            1 -> MsgNoBlock <$> decode
            t -> cborError $ "MsgBlock wrong tag: " <> show t

-- deriveSimpleBi is not happy with constructors without arguments
-- "fake" deriving as per `MempoolMsg`.
-- TODO: Shall we encode this as `CBOR` TkNull?
instance Bi MsgSubscribe1 where
    encode MsgSubscribe1 = encode (42 :: Word8)
    encodedSize _ = 2
    decode = decode @Word8 >>= \case
        42 -> pure MsgSubscribe1
        n  -> cborError $ "MsgSubscribe1 wrong byte:" <> show n

instance Bi MsgSubscribe where
    encode = \case
        MsgSubscribe          -> encode (42 :: Word8)
        MsgSubscribeKeepAlive -> encode (43 :: Word8)
    encodedSize _ = 2
    decode = decode @Word8 >>= \case
        42 -> pure MsgSubscribe
        43 -> pure MsgSubscribeKeepAlive
        n  -> cborError $ "MsgSubscribe wrong byte: " <> show n

----------------------------------------------------------------------------
-- Protocol version info and related
----------------------------------------------------------------------------

instance Bi HandlerSpec where
    encode input = case input of
        ConvHandler mname        ->
            encodeListLen 2 <> encode (0 :: Word8) <> encodeKnownCborDataItem mname
        UnknownHandler word8 bs  ->
            encodeListLen 2 <> encode word8 <> encodeUnknownCborDataItem (LBS.fromStrict bs)
    encodedSize input = case input of
        ConvHandler mname        ->
            2 + encodedKnownCborDataItemSize mname
        UnknownHandler word8 bs  ->
            let len = fromIntegral $ length bs
            in 2 + encodedSize word8 + withSize len 1 2 3 5 9 + encodedUnknownCborDataItemSize (fromBytes len)
    decode = do
        enforceSize "HandlerSpec" 2
        tag <- decode @Word8
        case tag of
          0 -> ConvHandler        <$> decodeKnownCborDataItem
          _ -> UnknownHandler tag <$> decodeUnknownCborDataItem

deriveSimpleBi ''VerInfo [
    Cons 'VerInfo [
        Field [| vIMagic        :: Int32        |],
        Field [| vIBlockVersion :: BlockVersion |],
        Field [| vIInHandlers   :: HandlerSpecs |],
        Field [| vIOutHandlers  :: HandlerSpecs |]
    ]]
