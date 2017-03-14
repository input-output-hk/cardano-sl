-- | Binary serialization of Pos.Types.Address
module Pos.Binary.Core.Address () where

import           Universum

import           Data.Binary.Get     (Get, getWord32be, getWord8, label)
import           Data.Binary.Put     (Put, putByteString, putWord32be, putWord8, runPut)
import           Data.Default        (def)
import           Data.Digest.CRC32   (CRC32 (..), crc32)

import           Pos.Binary.Class    (Bi (..), getRemainingByteString, getSmallWithLength,
                                      putSmallWithLength)
import           Pos.Binary.Crypto   ()
import           Pos.Core.Types      (AddrPkAttrs (..), Address (..))
import           Pos.Data.Attributes (getAttributes, putAttributes)

-- | Encode everything in an address except for CRC32
putAddressIncomplete :: Address -> Put
putAddressIncomplete = \case
    PubKeyAddress keyHash attrs -> do
        putWord8 0
        putSmallWithLength $ do
            put keyHash
            flip putAttributes attrs $ \case
                AddrPkAttrs Nothing -> []
                AddrPkAttrs (Just path) -> [(0, put path)]
    ScriptAddress scrHash -> do
        putWord8 1
        putSmallWithLength $
            put scrHash
    RedeemAddress keyHash -> do
        putWord8 2
        putSmallWithLength $ put keyHash
    UnknownAddressType t bs -> do
        putWord8 t
        putSmallWithLength $
            putByteString bs

-- | Decode everything except for CRC32
getAddressIncomplete :: Get Address
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
        t -> UnknownAddressType t <$> getRemainingByteString

instance CRC32 Address where
    crc32Update seed = crc32Update seed . runPut . putAddressIncomplete

instance Bi Address where
    get = label "Address" $ do
        addr <- getAddressIncomplete
        checksum <- getWord32be
        if checksum /= crc32 addr
            then fail "Address has invalid checksum!"
            else return addr
    put addr = do
        putAddressIncomplete addr
        putWord32be (crc32 addr)
