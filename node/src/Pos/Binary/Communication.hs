{-# LANGUAGE BinaryLiterals #-}

-- | Communication-related serialization -- messages mostly.

module Pos.Binary.Communication () where

import           Universum

import           Pos.Binary.Block                 ()
import           Pos.Binary.Class                 (Bi (..), Cons (..), Field (..),
                                                   decodeCborDataItem,
                                                   decodeCborDataItemTag, deriveSimpleBi,
                                                   encodeCborDataItem, encodeListLen,
                                                   enforceSize, serialize')
import           Pos.Block.Network.Types          (MsgBlock (..), MsgGetBlocks (..),
                                                   MsgGetHeaders (..), MsgHeaders (..))
import           Pos.Communication.Types.Protocol (HandlerSpec (..), HandlerSpecs,
                                                   MsgSubscribe (..), VerInfo (..))
import           Pos.Core                         (BlockVersion, HasCoreConstants,
                                                   HeaderHash)
import           Pos.Ssc.Class.Helpers            (SscHelpersClass)

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

instance (HasCoreConstants, SscHelpersClass ssc) => Bi (MsgHeaders ssc) where
  encode (MsgHeaders b) = encode b
  decode = MsgHeaders <$> decode

instance (HasCoreConstants, SscHelpersClass ssc) => Bi (MsgBlock ssc) where
  encode (MsgBlock b) = encode b
  decode = MsgBlock <$> decode

-- deriveSimpleBi is not happy with constructors without arguments
-- "fake" deriving as per `MempoolMsg`.
-- TODO: Shall we encode this as `CBOR` TkNull?
instance Bi MsgSubscribe where
  encode MsgSubscribe = encode (42 :: Word8)
  decode = do
    x <- decode @Word8
    when (x /= 42) $ fail "wrong byte"
    pure MsgSubscribe

----------------------------------------------------------------------------
-- Protocol version info and related
----------------------------------------------------------------------------

instance Bi HandlerSpec where
  encode input = case input of
    ConvHandler mname        -> encodeListLen 2 <> encode (0 :: Word8) <> encodeCborDataItem (serialize' mname)
    UnknownHandler word8 bs  -> encodeListLen 2 <> encode word8 <> encodeCborDataItem bs
  decode = do
    enforceSize "HandlerSpec" 2
    tag <- decode @Word8
    case tag of
      0 -> ConvHandler        <$> decodeCborDataItem
      _ -> UnknownHandler tag <$> (decodeCborDataItemTag *> decode)

deriveSimpleBi ''VerInfo [
    Cons 'VerInfo [
        Field [| vIMagic        :: Int32        |],
        Field [| vIBlockVersion :: BlockVersion |],
        Field [| vIInHandlers   :: HandlerSpecs |],
        Field [| vIOutHandlers  :: HandlerSpecs |]
    ]]
