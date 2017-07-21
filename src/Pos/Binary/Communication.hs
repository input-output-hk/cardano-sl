{-# LANGUAGE BinaryLiterals      #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Communication-related serialization -- messages mostly.

module Pos.Binary.Communication () where

import           Universum

import           Node.Message.Class               (MessageName (..))

import           Pos.Binary.Block                 ()
import           Pos.Binary.Class                 (Bi (..), Cons (..), Field (..), deriveSimpleBi,
                                                   serialize', encodeListLen, enforceSize, deserialize')
import           Pos.Block.Network.Types          (MsgBlock (..), MsgGetBlocks (..),
                                                   MsgGetHeaders (..), MsgHeaders (..))
import           Pos.Communication.Types.Protocol (HandlerSpec (..), HandlerSpecs,
                                                   VerInfo (..))
import           Pos.Core                         (BlockVersion, HeaderHash)
import           Pos.Ssc.Class.Helpers            (SscHelpersClass)

----------------------------------------------------------------------------
-- MessageName
----------------------------------------------------------------------------

deriving instance Bi MessageName

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

instance SscHelpersClass ssc => Bi (MsgHeaders ssc) where
  encode (MsgHeaders b) = encode b
  decode = MsgHeaders <$> decode

instance SscHelpersClass ssc => Bi (MsgBlock ssc) where
  encode (MsgBlock b) = encode b
  decode = MsgBlock <$> decode

----------------------------------------------------------------------------
-- Protocol version info and related
----------------------------------------------------------------------------

{- Encoding of HandlerSpec is as follows:

| Type                                 | Size     | Value    | Following data |
|--------------------------------------|----------|----------|----------------|
| <reserved for future usage>          | Fixed    | 00000000 | <none>         |
| ConvHandler m, m:UnsignedVarInt < 64 | Fixed    | 01xxxxxx | <none>         |
| ConvHandler m, m:Unknown             | Variable | 00000001 | MessageName    |
| UnknownHandler w8 bytes              | Variable | w8       | len, bytes     |

-}

instance Bi HandlerSpec where
  encode input = case input of
    ConvHandler mname        -> encodeListLen 2 <> encode (0 :: Word8) <> encode (serialize' mname)
    UnknownHandler word8 bs  -> encodeListLen 2 <> encode word8 <> encode bs
  decode = do
    enforceSize "HandlerSpec" 2
    tag <- decode @Word8
    case tag of
      0 -> ConvHandler . deserialize' <$> decode
      _ -> UnknownHandler tag         <$> decode

deriveSimpleBi ''VerInfo [
    Cons 'VerInfo [
        Field [| vIMagic        :: Int32        |],
        Field [| vIBlockVersion :: BlockVersion |],
        Field [| vIInHandlers   :: HandlerSpecs |],
        Field [| vIOutHandlers  :: HandlerSpecs |]
    ]]
