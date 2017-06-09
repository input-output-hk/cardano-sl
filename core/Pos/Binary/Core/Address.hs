-- | Binary serialization of Pos.Types.Address
module Pos.Binary.Core.Address () where

import           Universum

import           Data.Binary.Get     (Get, getWord32be)
import           Data.Binary.Put     (Put, putWord32be, runPut)
import           Data.Default        (def)
import           Data.Digest.CRC32   (CRC32 (..), crc32)
import           Pos.Binary.Class    (Bi (..), Peek, Poke, UnsignedVarInt (..), encode,
                                      label, put, putByteString, putSmallWithLength,
                                      putWord8)
import           Pos.Binary.Crypto   ()
import           Pos.Core.Types      (AddrPkAttrs (..), Address (..))
import           Pos.Data.Attributes (getAttributes, putAttributes)

-- -- | Encode everything in an address except for CRC32
putAddressIncomplete :: Address -> Poke ()
putAddressIncomplete = undefined
-- putAddressIncomplete = \case
--     PubKeyAddress keyHash attrs -> do
--         putWord8 0
--         putSmallWithLength $ do
--             put keyHash
--             flip putAttributes attrs $ \case
--                 AddrPkAttrs Nothing -> []
--                 AddrPkAttrs (Just path) -> [(0, put path)]
--     ScriptAddress scrHash -> do
--         putWord8 1
--         putSmallWithLength scrHash
--     RedeemAddress keyHash -> do
--         putWord8 2
--         putSmallWithLength keyHash
--     UnknownAddressType t bs -> do
--         putWord8 t
--         putSmallWithLength $ putByteString bs
--
-- -- | Decode everything except for CRC32
getAddressIncomplete :: Peek Address
getAddressIncomplete = undefined
-- getAddressIncomplete = do
--     tag <- getWord8
--     getSmallWithLength $ case tag of
--         0 -> let mapper 0 x = Just $
--                      get <&> \a -> x {addrPkDerivationPath = Just a}
--                  mapper _ _ = Nothing
--              in PubKeyAddress
--                     <$> get
--                     <*> getAttributes mapper Nothing def
--         1 -> ScriptAddress <$> get
--         2 -> RedeemAddress <$> get
--         t -> UnknownAddressType t <$> getRemainingByteString

instance CRC32 Address where
    crc32Update seed = crc32Update seed . encode

instance Bi Address where
    get = label "Address" $ do
       addr <- getAddressIncomplete
       UnsignedVarInt checksum <- get
       if checksum /= crc32 addr
           then fail "Address has invalid checksum!"
           else return addr
    put addr = do
        putAddressIncomplete addr
        put . UnsignedVarInt . crc32 $ addr
