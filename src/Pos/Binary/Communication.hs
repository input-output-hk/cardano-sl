{-# LANGUAGE ScopedTypeVariables #-}
-- | Communication-related serialization -- messages mostly.

module Pos.Binary.Communication () where

import           Universum

import           Data.Bits                        (Bits (..))
import qualified Data.ByteString.Lazy             as BSL
import           Node.Message                     (MessageName (..))

import           Pos.Binary.Block                 ()
import           Pos.Binary.Class                 (Bi (..), UnsignedVarInt (..),
                                                   decodeFull, encodeStrict,
                                                   getRemainingByteString,
                                                   getSmallWithLength, getWord8, label,
                                                   putByteString, putSmallWithLength,
                                                   putWord8)
import           Pos.Block.Network.Types          (MsgBlock (..), MsgGetBlocks (..),
                                                   MsgGetHeaders (..), MsgHeaders (..))
import           Pos.Communication.Types.Protocol (HandlerSpec (..), VerInfo (..))
import           Pos.Ssc.Class.Helpers            (SscHelpersClass)

----------------------------------------------------------------------------
-- MessageName
----------------------------------------------------------------------------

deriving instance Bi MessageName

-- TODO: move into each component

----------------------------------------------------------------------------
-- Blocks
----------------------------------------------------------------------------

instance Bi MsgGetHeaders where
    put (MsgGetHeaders f t) = put f >> put t
    get = label "MsgGetHeaders" $ MsgGetHeaders <$> get <*> get

instance Bi MsgGetBlocks where
    put (MsgGetBlocks f t) = put f >> put t
    get = label "MsgGetBlocks" $ MsgGetBlocks <$> get <*> get

instance SscHelpersClass ssc => Bi (MsgHeaders ssc) where
    put (MsgHeaders b) = put b
    get = label "MsgHeaders" $ MsgHeaders <$> get

instance SscHelpersClass ssc => Bi (MsgBlock ssc) where
    put (MsgBlock b) = put b
    get = label "MsgBlock" $ MsgBlock <$> get

----------------------------------------------------------------------------
-- Protocol version info and related
----------------------------------------------------------------------------


-- Encoding of HandlerSpec is as follow:
--
-- | Type                                        | Size     | Value     | Following data |
-- |---------------------------------------------|----------|-----------|----------------|
-- | <reserved for future usage>                 | Fixed    | 0000 0000 | none           |
-- | ConvHandler m where m : UnsignedVarInt < 64 | Fixed    | 01xx xxxx | none           |
-- | ConvHandler m where m : Unknown             | Variable | 0000 0001 | EncodeString   |
-- | UnknownHandler w8 bs                        | Variable | w8        | bs             |
instance Bi HandlerSpec where
    put (ConvHandler (MessageName m)) =
        case decodeFull $ BSL.fromStrict m of
            Right (UnsignedVarInt a)
                | a < 64 -> putWord8 (0x40 .|. (fromIntegral (a :: Word) .&. 0x3f))
            _ -> putWord8 1 >> putSmallWithLength (put m)
    put (UnknownHandler t b) =
        putWord8 t >> putSmallWithLength (putByteString b)
    get = label "HandlerSpec" $ getWord8 >>= \case
        0                        -> pure $ UnknownHandler 0 mempty
        1                        -> getSmallWithLength (ConvHandler <$> get)
        t | (t .&. 0xc0) == 0x40 ->
            pure . ConvHandler . MessageName . encodeStrict $
            UnsignedVarInt (fromIntegral (t .&. 0x3f) :: Word)
          | otherwise            ->
            getSmallWithLength (UnknownHandler t <$> getRemainingByteString)

instance Bi VerInfo where
    put VerInfo {..} = put vIMagic
                    <> put vIBlockVersion
                    <> put vIInHandlers
                    <> put vIOutHandlers
    get = label "VerInfo" $ VerInfo <$> get <*> get <*> get <*> get
