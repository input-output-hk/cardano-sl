-- | Binary serialization of Pos.Types.Address
module Pos.Binary.Core.Address () where

import           Universum

import           Data.Default        (def)
import           Data.Digest.CRC32   (CRC32 (..), crc32)
import           Pos.Binary.Class    (Bi (..), Peek, Poke, PokeWithSize, Size (..),
                                      convertToSizeNPut, encodeWithS, getBytes,
                                      getSmallWithLength, getWord8, label, labelS,
                                      putBytesS, putField, putS, putSmallWithLengthS,
                                      putWord8S)
import           Pos.Binary.Crypto   ()
import           Pos.Core.Types      (AddrPkAttrs (..), Address (..))
import           Pos.Data.Attributes (getAttributes, putAttributesS)

-- | Encode everything in an address except for CRC32
sizeNPutAddressIncomplete :: (Size Address, Address -> Poke ())
sizeNPutAddressIncomplete = convertToSizeNPut toBi
  where
    toBi :: Address -> PokeWithSize ()
    toBi = \case
        PubKeyAddress keyHash attrs ->
            putWithTag 0 $
                putS keyHash <>
                putAttributesS addrToList attrs
        ScriptAddress scrHash ->
            putWithTag 1 $ putS scrHash
        RedeemAddress keyHash ->
            putWithTag 2 $ putS keyHash
        UnknownAddressType t bs ->
            putWithTag t $ putBytesS bs

    -- | Put tag, then length of X, then X itself
    putWithTag :: Word8 -> PokeWithSize () -> PokeWithSize ()
    putWithTag t x = putWord8S t <> putSmallWithLengthS x

    addrToList :: AddrPkAttrs -> [(Word8, PokeWithSize ())]
    addrToList = \case
        AddrPkAttrs Nothing -> []
        AddrPkAttrs (Just path) -> [(0, putS path)]

-- | Decode everything except for CRC32
getAddressIncomplete :: Peek Address
getAddressIncomplete = do
    tag <- getWord8
    getSmallWithLength $ \len -> case tag of
        0 -> do
            let mapper 0 x = Just $ get <&> \a -> x {addrPkDerivationPath = Just a}
                mapper _ _ = Nothing
            PubKeyAddress <$> get <*> getAttributes mapper Nothing def
        1 -> ScriptAddress <$> get
        2 -> RedeemAddress <$> get
        t -> UnknownAddressType t <$> getBytes (fromIntegral len)

instance CRC32 Address where
    crc32Update seed = crc32Update seed . encodeWithS sizeNPutAddressIncomplete

instance Bi Address where
    sizeNPut = labelS "Address" $
        sizeNPutAddressIncomplete <>
        putField crc32
    get = label "Address" $ do
       addr <- getAddressIncomplete
       checksum <- get
       if checksum /= crc32 addr
           then fail "Address has invalid checksum!"
           else return addr
