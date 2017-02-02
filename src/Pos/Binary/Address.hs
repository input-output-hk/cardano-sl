-- | Binary serialization of Pos.Types.Address
module Pos.Binary.Address () where

import           Data.Binary.Get   (Get, getWord32be, getWord8)
import           Data.Binary.Put   (Put, putByteString, putWord32be, putWord8, runPut)
import           Data.Digest.CRC32 (CRC32 (..), crc32)
import           Universum         hiding (putByteString)

import           Pos.Binary.Class  (Bi (..))
import           Pos.Types.Address (Address (..))
import           Pos.Util.Binary   (getRemainingByteString, getSmallWithLength,
                                    putSmallWithLength)

-- | Encode everything in an address except for CRC32
putAddressIncomplete :: Address -> Put
putAddressIncomplete = \case
    PubKeyAddress keyHash -> do
        putWord8 0
        putSmallWithLength $
            put keyHash
    ScriptAddress scrHash -> do
        putWord8 1
        putSmallWithLength $
            put scrHash
    UnknownAddressType t bs -> do
        putWord8 t
        putSmallWithLength $
            putByteString bs

-- | Decode everything except for CRC32
getAddressIncomplete :: Get Address
getAddressIncomplete = do
    tag <- getWord8
    case tag of
        0 -> getSmallWithLength (PubKeyAddress <$> get)
        1 -> getSmallWithLength (ScriptAddress <$> get)
        t -> getSmallWithLength (UnknownAddressType t <$>
                                 getRemainingByteString)

instance CRC32 Address where
    crc32Update seed = crc32Update seed . runPut . putAddressIncomplete

instance Bi Address where
    get = do
        addr <- getAddressIncomplete
        checksum <- getWord32be
        if checksum /= crc32 addr
            then fail "Address has invalid checksum!"
            else return addr
    put addr = do
        putAddressIncomplete addr
        putWord32be (crc32 addr)
