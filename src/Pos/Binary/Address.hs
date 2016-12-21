-- | Binary serialization of Pos.Types.Address

module Pos.Binary.Address () where

import           Control.Monad.Fail (fail)
import           Data.Binary.Get    (getWord32be, getWord8)
import           Data.Binary.Put    (putWord32be, putWord8)
import           Data.Bits          (xor, (.|.))
import           Data.Digest.CRC32  (crc32)
import           Universum

import           Pos.Binary.Class   (Bi (..))
import           Pos.Types.Address  (Address (..), AddressDestination (..),
                                     AddressVersion (..))

instance Bi Address where
    get = do
        ver <- getWord8
        addr <- if ver < 128
                then do addrDestKeyHash <- get
                        let addrVersion = AddressVersion ver
                        let addrDestination = PubKeyDestination{..}
                        let addrDistribution = []
                        return Address{..}
                else do addrDestScriptHash <- get
                        let addrVersion = AddressVersion (ver `xor` 128)
                        let addrDestination = ScriptDestination{..}
                        addrDistribution <- get
                        return Address{..}
        theirChecksum <- getWord32be
        if theirChecksum /= crc32 addr
            then fail "Address has invalid checksum!"
            else return addr
    put addr = do
        let AddressVersion ver = addrVersion addr
        case addrDestination addr of
            PubKeyDestination{..} -> do
                putWord8 ver
                put addrDestKeyHash
            ScriptDestination{..} -> do
                putWord8 (ver .|. 128)
                put addrDestScriptHash
                put (addrDistribution addr)
        putWord32be $ crc32 addr
