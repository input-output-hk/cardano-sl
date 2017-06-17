-- | Binary serialization of Pos.Types.Address
module Pos.Binary.Core.Address () where

import           Universum

import           Data.Default        (def)
import           Data.Digest.CRC32   (CRC32 (..), crc32)
import           Pos.Binary.Class    (Bi (..), Peek, Poke, PokeWithSize, Size (..),
                                      UnsignedVarInt (..), convertToSizeNPut, encode,
                                      getSmallWithLength, getWord8, label, putField, putS,
                                      putSmallWithLengthS, putWord8S)
import           Pos.Binary.Crypto   ()
import           Pos.Core.Types      (AddrPkAttrs (..), Address (..))
import           Pos.Data.Attributes (getAttributes, putAttributesWithSize)

-- | Encode everything in an address except for CRC32
sizeNPutAddressIncomplete :: (Size Address, Address -> Poke ())
sizeNPutAddressIncomplete = convertToSizeNPut toBi
  where
    toBi :: Address -> PokeWithSize ()
    toBi = \case
        PubKeyAddress keyHash attrs ->
            putWord8S 0 <>
            putSmallWithLengthS (
                putS keyHash <>
                flip putAttributesWithSize attrs addrToList
            )
        ScriptAddress scrHash   -> putWord8S 1 <> putSmallS scrHash
        RedeemAddress keyHash   -> putWord8S 2 <> putSmallS keyHash
        UnknownAddressType t bs -> putWord8S t <> putSmallS bs
    putSmallS :: Bi a => a -> PokeWithSize ()
    putSmallS = putSmallWithLengthS . putS

    addrToList :: AddrPkAttrs -> [(Word8, PokeWithSize ())]
    addrToList = \case
        AddrPkAttrs Nothing -> []
        AddrPkAttrs (Just path) -> [(0, putS path)]

-- | Decode everything except for CRC32
getAddressIncomplete :: Peek Address
getAddressIncomplete = do
    tag <- getWord8
    getSmallWithLength $ case tag of
        0 -> let mapper 0 x = Just $
                     get <&> \a -> x {addrPkDerivationPath = Just a}
                 mapper _ _ = Nothing
             in PubKeyAddress
                    <$> get
                    <*> getAttributes mapper Nothing def
        1 -> ScriptAddress <$> get
        2 -> RedeemAddress <$> get
        t -> UnknownAddressType t <$> get

instance CRC32 Address where
    crc32Update seed = crc32Update seed . encode

instance Bi Address where
    sizeNPut = sizeNPutAddressIncomplete
            <> putField (UnsignedVarInt . crc32)
    get = label "Address" $ do
       addr <- getAddressIncomplete
       UnsignedVarInt checksum <- get
       if checksum /= crc32 addr
           then fail "Address has invalid checksum!"
           else return addr
