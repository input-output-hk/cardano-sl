-- | Binary serialization of Pos.Types.Address

module Pos.Binary.Address () where

import           Control.Monad.Fail (fail)
import           Data.Binary.Get    (getWord32be, getWord8)
import           Data.Binary.Put    (putWord32be, putWord8)
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
        ver <- getWord8
        addrHash <- get
        let addr = PubKeyAddress ver addrHash
            ourChecksum = crc32 addr
        theirChecksum <- getWord32be
        if theirChecksum /= ourChecksum
            then fail "Address has invalid checksum!"
            else pure addr
    put addr@PubKeyAddress {..} = do
        putWord8 addrVersion
        put addrHash
        putWord32be $ crc32 addr
