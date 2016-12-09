module Pos.Types.Address
       ( Address (..)
       , addressF
       , checkPubKeyAddress
       , checkScriptAddress
       , makePubKeyAddress
       , makeScriptAddress

         -- * Internals
       , AddressHash(..)
       , addressHash
       , unsafeAddressHash
       , curAddrVersion
       ) where

import           Control.Lens           (view, _3)
import           Control.Monad.Fail     (fail)
import           Crypto.Hash            (Blake2s_224, Digest, SHA3_256, hashlazy)
import qualified Crypto.Hash            as CryptoHash
import           Data.Aeson             (ToJSON (toJSON))
import           Data.Binary            (Binary (..))
import qualified Data.Binary            as Bi
import qualified Data.Binary.Get        as Bi (getWord32be)
import qualified Data.Binary.Put        as Bi (putWord32be)
import           Data.ByteString.Base58 (Alphabet (..), bitcoinAlphabet, decodeBase58,
                                         encodeBase58)
import qualified Data.ByteString.Char8  as BSC (elem, unpack)
import qualified Data.ByteString.Lazy   as BSL (fromStrict, toStrict)
import           Data.Char              (isSpace)
import           Data.Digest.CRC32      (CRC32 (..))
import           Data.Hashable          (Hashable (..))
import           Data.List              (span)
import           Data.Text.Buildable    (Buildable)
import qualified Data.Text.Buildable    as Buildable
import           Formatting             (Format, build, sformat)
import           Prelude                (String, readsPrec, show)
import           Universum              hiding (show)

import           Pos.Crypto             (AbstractHash (AbstractHash), PublicKey)
import           Pos.Script             (Script)

-- | Address versions are here for dealing with possible backwards
-- compatibility issues in the future
type AddressVersion = Word8

curAddrVersion :: AddressVersion
curAddrVersion = 0

-- | Address is where you can send coins.
data Address
    = PubKeyAddress {
          addrVersion :: !AddressVersion,
          addrKeyHash :: !(AddressHash PublicKey) }
    | ScriptAddress {
          addrVersion    :: !AddressVersion,
          addrScriptHash :: !(AddressHash Script) }
    deriving (Eq, Generic, Ord)

instance CRC32 Address where
    crc32Update seed PubKeyAddress {..} =
        crc32Update (crc32Update seed [0, addrVersion]) $
        Bi.encode addrKeyHash
    crc32Update seed ScriptAddress {..} =
        crc32Update (crc32Update seed [1, addrVersion]) $
        Bi.encode addrScriptHash

instance Binary Address where
    get = do
        addrVersion <- Bi.getWord8
        tag <- Bi.getWord8
        addr <- case tag of
            0 -> do addrKeyHash <- get
                    return PubKeyAddress {..}
            1 -> do addrScriptHash <- get
                    return ScriptAddress {..}
            _ -> fail ("get@Address: unknown tag " ++ show tag)
        theirChecksum <- Bi.getWord32be
        if theirChecksum /= crc32 addr
            then fail "Address has invalid checksum!"
            else return addr
    put addr = do
        Bi.putWord8 (addrVersion addr)
        case addr of
            PubKeyAddress {..} -> Bi.putWord8 0 >> put addrKeyHash
            ScriptAddress {..} -> Bi.putWord8 1 >> put addrScriptHash
        Bi.putWord32be $ crc32 addr

instance Hashable Address where
    hashWithSalt s = hashWithSalt s . Bi.encode

-- | Currently we gonna use Bitcoin alphabet for representing addresses in base58
addrAlphabet :: Alphabet
addrAlphabet = bitcoinAlphabet

addrToBase58 :: Address -> ByteString
addrToBase58 = encodeBase58 addrAlphabet . BSL.toStrict . Bi.encode

instance Show Address where
    show = BSC.unpack . addrToBase58

instance Buildable Address where
    build = Buildable.build . decodeUtf8 @Text . addrToBase58

instance NFData Address

instance ToJSON Address where
    toJSON = toJSON . sformat build

instance Read Address where
    readsPrec _ str =
        let trimmedStr = dropWhile isSpace str
            (addrStr, rest) = span (`BSC.elem` unAlphabet addrAlphabet) trimmedStr
            eAddr = decodeAddress $ encodeUtf8 addrStr
        in case eAddr of
               Left _     -> []
               Right addr -> [(addr, rest)]

-- | A function which decodes base58 address from given ByteString
decodeAddress :: ByteString -> Either String Address
decodeAddress bs = do
    let base58Err = "Invalid base58 representation of address"
        takeErr = toString . view _3
        takeRes = view _3
    dbs <- maybeToRight base58Err $ decodeBase58 addrAlphabet bs
    bimap takeErr takeRes $ Bi.decodeOrFail $ BSL.fromStrict dbs

-- | A function for making an address from 'PublicKey'
makePubKeyAddress :: PublicKey -> Address
makePubKeyAddress = PubKeyAddress curAddrVersion . addressHash

-- | A function for making an address from a validation script
makeScriptAddress :: Script -> Address
makeScriptAddress = ScriptAddress curAddrVersion . addressHash

-- | Check if given 'Address' is created from given 'PublicKey'
checkPubKeyAddress :: PublicKey -> Address -> Bool
checkPubKeyAddress pk PubKeyAddress{..} = addrKeyHash == addressHash pk
checkPubKeyAddress _ _                  = False

-- | Check if given 'Address' is created from given validation script
checkScriptAddress :: Script -> Address -> Bool
checkScriptAddress scr ScriptAddress{..} = addrScriptHash == addressHash scr
checkScriptAddress _ _                   = False

-- | Specialized formatter for 'Address'.
addressF :: Format r (Address -> r)
addressF = build

----------------------------------------------------------------------------
-- Hashing
----------------------------------------------------------------------------
type AddressHash = AbstractHash Blake2s_224

unsafeAddressHash :: Binary a => a -> AddressHash b
unsafeAddressHash = AbstractHash . secondHash . firstHash
  where
    firstHash :: Binary a => a -> Digest SHA3_256
    firstHash = hashlazy . Bi.encode
    secondHash :: Digest SHA3_256 -> Digest Blake2s_224
    secondHash = CryptoHash.hash

addressHash :: Binary a => a -> AddressHash a
addressHash = unsafeAddressHash
