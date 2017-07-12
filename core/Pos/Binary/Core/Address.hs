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
import qualified Pos.Binary.Cbor     as Cbor
import           Pos.Binary.Crypto   ()
import           Pos.Core.Types      (AddrPkAttrs (..), Address (..))
import           Pos.Data.Attributes (getAttributes, putAttributesS)

{- NOTE: Address serialization
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

An address is serialized as follows:

    <one-byte tag><length of content><content>

This lets us have backwards compatibility. For instance, if a newer version
of CSL adds an address with tag 5:

    data Address
        = ...
        | SuperAddress A B C

then older versions would deserialize it as follows:

    UnknownAddressType 5 <some bytes>

The length is needed because otherwise we wouldn't know where the address
ends and the next part of the structure begins (if an address is embedded
into a bigger structure).

Moreover, since we want *any* address to be potentially deserializable as
UnknownAddressType, we add length to already existing addresses
(PubKeyAddress, ScriptAddress, etc) as well even though it's not strictly
necessary (because there are no previous versions of CSL to be
backwards-compatible with and so we could special-case those types of
addresses and save a byte).
-}

-- | Encode everything in an address except for CRC32
sizeNPutAddressIncomplete :: (Size Address, Address -> Poke ())
sizeNPutAddressIncomplete = convertToSizeNPut toBi
  where
    toBi :: Address -> PokeWithSize ()
    toBi = \case
        -- It's important that we use 'putWithTag' for all branches.
        -- See the note above.
        PubKeyAddress keyHash attrs ->
            putWithTag 0 $
                putS keyHash <>
                putAttributesS addrToList attrs
        ScriptAddress scrHash ->
            putWithTag 1 $ putS scrHash
        RedeemAddress keyHash ->
            putWithTag 2 $ putS keyHash
        UnknownAddressType t bs ->
            -- It's important that it's 'putBytesS' and not just 'putS'.
            -- See the note above.
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

-- Just a stub as @arybczak is working on it.
instance Cbor.Bi Address where
  encode = error "Address encode: unimplemented"
  decode = fail  "Address decode: unimplemented"
