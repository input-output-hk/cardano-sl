-- | Binary serialization of Pos.Types.Address
module Pos.Binary.Address () where

import           Data.Binary.Get   (Get, getWord32be, getWord8)
import           Data.Binary.Put   (Put, putWord32be, putWord8, runPut)
import           Data.Digest.CRC32 (CRC32 (..), crc32)
import           Universum

import           Pos.Binary.Class  (Bi (..))
import           Pos.Types.Address (Address (..))

-- | Encode everything in an address except for CRC32
putAddressIncomplete :: Address -> Put
putAddressIncomplete = \case
    PubKeyAddress keyHash -> do
        putWord8 0
        put keyHash
    ScriptAddress scrHash -> do
        putWord8 1
        put scrHash

-- | Decode everything except for CRC32
getAddressIncomplete :: Get Address
getAddressIncomplete = do
    tag <- getWord8
    case tag of
        0 -> PubKeyAddress <$> get
        1 -> ScriptAddress <$> get
        _ -> fail ("getAddressIncomplete: unknown tag " ++ show tag)

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
