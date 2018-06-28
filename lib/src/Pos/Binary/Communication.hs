{-# LANGUAGE BinaryLiterals #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Communication-related serialization -- messages mostly.

module Pos.Binary.Communication
    ( serializeMsgSerializedBlock
    , serializeMsgStreamBlock
    ) where

import           Universum

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import           Pos.Binary.Class (Bi (..), Cons (..), Field (..),
                     decodeKnownCborDataItem, decodeUnknownCborDataItem,
                     deriveSimpleBi, encodeKnownCborDataItem, encodeListLen,
                     encodeUnknownCborDataItem, enforceSize, serialize,
                     serialize')
import           Pos.Block.BHelpers ()
import           Pos.Block.Network (MsgBlock (..), MsgSerializedBlock (..),
                     MsgStreamBlock (..))
import           Pos.Core (BlockVersion)
import           Pos.DB.Class (Serialized (..))
import           Pos.Infra.Communication.Types.Protocol (HandlerSpec (..),
                     HandlerSpecs, MsgSubscribe (..), MsgSubscribe1 (..),
                     VerInfo (..))
import           Pos.Util.Util (cborError)

-- TODO: move into each component

----------------------------------------------------------------------------
-- Blocks
----------------------------------------------------------------------------

-- Serialize `MsgSerializedBlock` with the property
-- ```
-- serialize (MsgBlock b) = serializeMsgSerializedBlock (MsgSerializedBlock $ serialize b)
-- ```
serializeMsgSerializedBlock :: MsgSerializedBlock -> BS.ByteString
serializeMsgSerializedBlock (MsgSerializedBlock b)   = "\x82\x0" <> unSerialized b
serializeMsgSerializedBlock (MsgNoSerializedBlock t) = serialize' (MsgNoBlock t)

-- Serialize `MsgSerializedBlock` with the property
-- ```
-- serialize (MsgStreamBlock b) = serializeMsgStreamBlock (MsgSerializedBlock $ serialize b)
-- ```
serializeMsgStreamBlock :: MsgSerializedBlock -> LBS.ByteString
serializeMsgStreamBlock (MsgSerializedBlock b)   = "\x82\x0" <> LBS.fromStrict (unSerialized b)
serializeMsgStreamBlock (MsgNoSerializedBlock t) = serialize (MsgStreamNoBlock t)

-- deriveSimpleBi is not happy with constructors without arguments
-- "fake" deriving as per `MempoolMsg`.
-- TODO: Shall we encode this as `CBOR` TkNull?
instance Bi MsgSubscribe1 where
    encode MsgSubscribe1 = encode (42 :: Word8)
    decode = decode @Word8 >>= \case
        42 -> pure MsgSubscribe1
        n  -> cborError $ "MsgSubscribe1 wrong byte:" <> show n

instance Bi MsgSubscribe where
    encode = \case
        MsgSubscribe          -> encode (42 :: Word8)
        MsgSubscribeKeepAlive -> encode (43 :: Word8)
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
