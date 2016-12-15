-- | Binary serialization of Pos.Types.Address

module Pos.Binary.Address () where

import           Control.Monad.Fail (fail)
import           Data.Binary.Get    (getWord32be, getWord8)
import           Data.Binary.Put    (putWord32be, putWord8)
import           Data.Bits          ((.|.))
import           Data.Digest.CRC32  (crc32)
import           Universum

import           Pos.Binary.Class   (Bi (..))
import           Pos.Types.Address  (Address (..))

-- Yes, I'm not joking. Address is something that essentially is
-- serialized to exist, so putting (Bi Address) => constraint
-- everywhere appeared too tedious for me, so there'll be just a
-- module here.

instance Bi Address where
    get = do
        addrVersion <- getWord8
        addr <- if addrVersion < 128
                then do addrKeyHash <- get
                        return PubKeyAddress {..}
                else do addrScriptHash <- get
                        return ScriptAddress {..}
        theirChecksum <- getWord32be
        if theirChecksum /= crc32 addr
            then fail "Address has invalid checksum!"
            else return addr
    put addr = do
        let ver = addrVersion addr
        case addr of
            PubKeyAddress {..} -> putWord8 ver >> put addrKeyHash
            ScriptAddress {..} -> putWord8 (ver .|. 128) >> put addrScriptHash
        putWord32be $ crc32 addr
