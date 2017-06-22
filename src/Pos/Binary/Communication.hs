{-# LANGUAGE BinaryLiterals      #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Communication-related serialization -- messages mostly.

module Pos.Binary.Communication () where

import           Universum

import           Data.Bits                        (Bits (..))
import           Node.Message.Class               (MessageName (..))

import           Pos.Binary.Block                 ()
import           Pos.Binary.Class                 (Bi (..), Cons (..), Field (..),
                                                   UnsignedVarInt (..), convertToSizeNPut,
                                                   decodeFull, deriveSimpleBi, encode,
                                                   getSmallWithLength, getWord8, label,
                                                   labelS, putField, putS,
                                                   putSmallWithLengthS, putWord8S)
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
    sizeNPut = labelS "MsgHeaders" $ putField $ \(MsgHeaders b) -> b
    get = label "MsgHeaders" $ MsgHeaders <$> get

instance SscHelpersClass ssc => Bi (MsgBlock ssc) where
    sizeNPut = labelS "MsgBlock" $ putField $ \(MsgBlock b) -> b
    get = label "MsgBlock" $ MsgBlock <$> get

----------------------------------------------------------------------------
-- Protocol version info and related
----------------------------------------------------------------------------

--
-- Encoding of HandlerSpec is as follows:
--
--  | Type                                 | Size     | Value     | Following data   |
--  |--------------------------------------|----------|-----------|------------------|
--  | <reserved for future usage>          | Fixed    | 0000 0000 | <none>           |
--  | ConvHandler m, m:UnsignedVarInt < 64 | Fixed    | 01xx xxxx | <none>           |
--  | ConvHandler m, m:Unknown             | Variable | 0000 0001 | len,MessageName  |
--  | UnknownHandler w8 bs                 | Variable | w8        | len,bs           |
--
instance Bi HandlerSpec where
    sizeNPut = labelS "HandlerSpec" $ convertToSizeNPut f
      where
        -- CSL-1122: finish with binary literals
        f (ConvHandler (MessageName m)) =
            case decodeFull m of
                Right (UnsignedVarInt a)
                    | a < 64 -> putWord8S (0x40 .|. (fromIntegral (a :: Word) .&. 0x3f))
                _ -> putWord8S 1 <> putSmallWithLengthS (putS m)
        -- CSL-1122: putSmallWithLengthS shouldn't be used on bytestrings
        -- because bytestring serialization already puts length
        f (UnknownHandler t b) = putWord8S t <> putSmallWithLengthS (putS b)

    get = label "HandlerSpec" $ getWord8 >>= \case
        -- 0000 0000: reserved
        0 -> pure $ UnknownHandler 0 mempty
        -- 0000 0001: ConvHandler with a message name
        1 -> getSmallWithLength (\_ -> ConvHandler <$> get)
        -- 01xx xxxx: ConvHandler (MessageName xxxxxx)
        t | (t .&. 0b11000000) == 0b01000000 ->
            pure . ConvHandler . MessageName . encode $
            UnsignedVarInt (fromIntegral (t .&. 0b00111111) :: Word)
        -- none of the above: unknown handler
          | otherwise -> UnknownHandler t <$> getSmallWithLength (\_ -> get)

deriveSimpleBi ''VerInfo [
    Cons 'VerInfo [
        Field [| vIMagic        :: Int32        |],
        Field [| vIBlockVersion :: BlockVersion |],
        Field [| vIInHandlers   :: HandlerSpecs |],
        Field [| vIOutHandlers  :: HandlerSpecs |]
    ]]
