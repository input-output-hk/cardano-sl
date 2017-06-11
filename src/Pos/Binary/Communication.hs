{-# LANGUAGE ScopedTypeVariables #-}
-- | Communication-related serialization -- messages mostly.

module Pos.Binary.Communication () where

import           Universum

import           Data.Bits                        (Bits (..))
import           Node.Message                     (MessageName (..))

import           Pos.Binary.Block                 ()
import           Pos.Binary.Class                 (Bi (..), UnsignedVarInt (..),
                                                   convertToSizeNPut, decodeFull, encode,
                                                   getSmallWithLength, getWord8, label,
                                                   pokeWithSize, putField,
                                                   putSmallWithLengthS, putWord8WithSize)
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
    sizeNPut = putField mghFrom <> putField mghTo
    get = label "MsgGetHeaders" $ MsgGetHeaders <$> get <*> get

instance Bi MsgGetBlocks where
    sizeNPut = putField mgbFrom <> putField mgbTo
    get = label "MsgGetBlocks" $ MsgGetBlocks <$> get <*> get

instance SscHelpersClass ssc => Bi (MsgHeaders ssc) where
    sizeNPut = putField $ \(MsgHeaders b) -> b
    get = label "MsgHeaders" $ MsgHeaders <$> get

instance SscHelpersClass ssc => Bi (MsgBlock ssc) where
    sizeNPut = putField $ \(MsgBlock b) -> b
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
    sizeNPut = convertToSizeNPut f
      where
        f (ConvHandler (MessageName m)) =
            case decodeFull m of
                Right (UnsignedVarInt a)
                    | a < 64 -> putWord8WithSize (0x40 .|. (fromIntegral (a :: Word) .&. 0x3f))
                _ -> putWord8WithSize 1 <> putSmallWithLengthS (pokeWithSize m)
        f (UnknownHandler t b) = putWord8WithSize t <> pokeWithSize b
    get = label "HandlerSpec" $ getWord8 >>= \case
        0                        -> pure $ UnknownHandler 0 mempty
        1                        -> getSmallWithLength (ConvHandler <$> get)
        t | (t .&. 0xc0) == 0x40 ->
            pure . ConvHandler . MessageName . encode $
            UnsignedVarInt (fromIntegral (t .&. 0x3f) :: Word)
          | otherwise            ->
            getSmallWithLength (UnknownHandler t <$> getSmallWithLength get)

instance Bi VerInfo where
    sizeNPut =
        putField vIMagic <>
        putField vIBlockVersion <>
        putField vIInHandlers <>
        putField vIOutHandlers
    get = label "VerInfo" $ VerInfo <$> get <*> get <*> get <*> get
