-- | Binary serialization of Pos.Types.Address
module Pos.Binary.Core.Address () where

import           Universum

import           Data.Default        (def)
import           Data.Digest.CRC32   (CRC32 (..), crc32)
import           Pos.Binary.Class    (Bi (..), Peek, Poke, PokeWithSize, Size (..),
                                      UnsignedVarInt (..), encode, getSize,
                                      getSmallWithLength, getWord8, label, pokeWithSize,
                                      put, putField, putSmallWithLength, putWord8)
import           Pos.Binary.Crypto   ()
import           Pos.Core.Types      (AddrPkAttrs (..), Address (..))
import           Pos.Data.Attributes (getAttributes, putAttributesWithSize,
                                      sizeAttributes)

addrToList :: AddrPkAttrs -> [(Word8, PokeWithSize ())]
addrToList = \case
    AddrPkAttrs Nothing -> []
    AddrPkAttrs (Just path) -> [(0, pokeWithSize path)]

-- | Encode everything in an address except for CRC32
sizeNputAddressIncomplete :: (Size Address, Address -> Poke ())
sizeNputAddressIncomplete = (sizeAddr, putAddr)
  where
    sizeAddr = VarSize $ \x -> 1 + case x of
        PubKeyAddress keyHash attrs -> getSize keyHash + sizeAttributes addrToList attrs
        ScriptAddress scrHash       -> getSize scrHash
        RedeemAddress keyHash       -> getSize keyHash
        UnknownAddressType _ bs     -> getSize bs
    putAddr = \case
        PubKeyAddress keyHash attrs -> do
            putWord8 0
            putSmallWithLength $
                pokeWithSize keyHash *>
                flip putAttributesWithSize attrs addrToList
        ScriptAddress scrHash -> do
            putWord8 1
            putSmall scrHash
        RedeemAddress keyHash -> do
            putWord8 2
            putSmall keyHash
        UnknownAddressType t bs -> do
            putWord8 t
            putSmall bs
    putSmall :: Bi a => a -> Poke ()
    putSmall = putSmallWithLength . pokeWithSize

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
    sizeNPut = sizeNputAddressIncomplete
            <> putField (UnsignedVarInt . crc32)
    get = label "Address" $ do
       addr <- getAddressIncomplete
       UnsignedVarInt checksum <- get
       if checksum /= crc32 addr
           then fail "Address has invalid checksum!"
           else return addr
