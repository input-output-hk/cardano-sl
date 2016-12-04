module Pos.Types.Address
       ( Address (..)
       , addressF
       , makePubKeyAddress
       , checkPubKeyAddress
       , addrAlphabet
       ) where

import           Data.Aeson             (ToJSON (toJSON))
import           Data.Binary            (Binary (..))
import qualified Data.Binary            as Binary
import qualified Data.Binary.Get        as Binary (getWord32be)
import qualified Data.Binary.Put        as Binary (putWord32be)
import           Data.ByteString.Base58 (Alphabet, bitcoinAlphabet, encodeBase58)
import qualified Data.ByteString.Char8  as BSC (unpack)
import qualified Data.ByteString.Lazy   as BSL (toStrict)
import           Data.Digest.CRC32      (CRC32 (..))
import           Data.Hashable          (Hashable (..))
import           Data.Text.Buildable    (Buildable)
import qualified Data.Text.Buildable    as Buildable
import           Formatting             (Format, build, sformat)
import           Prelude                (show)
import           Universum              hiding (show)

import           Pos.Crypto             (AddressHash, PublicKey, addressHash)

-- | Address versions are here for dealing with possible backwards
-- compatibility issues in the future
type AddressVersion = Word8

curAddrVersion :: AddressVersion
curAddrVersion = 0

-- | Address is where you can send coins.
-- It's not `newtype` because in the future there will be `ScriptAddress`es
-- as well.
data Address = PubKeyAddress
    { addrVersion :: !AddressVersion
    , addrHash    :: !(AddressHash PublicKey)
    } deriving (Eq, Generic, Ord)

instance CRC32 Address where
    crc32Update seed PubKeyAddress {..} =
        crc32Update (crc32Update seed [addrVersion]) $ Binary.encode addrHash

instance Binary Address where
    get = do
        ver <- Binary.getWord8
        addrHash <- get
        let addr = PubKeyAddress ver addrHash
            ourChecksum = crc32 addr
        theirChecksum <- Binary.getWord32be
        if theirChecksum /= ourChecksum
            then panic "Pos.Types.Address.get: invalid checksum!"
            else return addr
    put addr@PubKeyAddress {..} = do
        Binary.putWord8 addrVersion
        put addrHash
        Binary.putWord32be $ crc32 addr

instance Hashable Address where
    hashWithSalt s = hashWithSalt s . Binary.encode

-- | Currently we gonna use Bitcoin alphabet for representing addresses in base58
addrAlphabet :: Alphabet
addrAlphabet = bitcoinAlphabet

addrToBase58 :: Address -> ByteString
addrToBase58 = encodeBase58 addrAlphabet . BSL.toStrict . Binary.encode

instance Show Address where
    show = BSC.unpack . addrToBase58

instance Buildable Address where
    build = Buildable.build . decodeUtf8 @Text . addrToBase58

instance NFData Address

instance ToJSON Address where
    toJSON = toJSON . sformat build

-- | A function for making an address from PublicKey
makePubKeyAddress :: PublicKey -> Address
makePubKeyAddress = PubKeyAddress curAddrVersion . addressHash

-- | Check if given `Address` is created from given `PublicKey`
checkPubKeyAddress :: PublicKey -> Address -> Bool
checkPubKeyAddress pk PubKeyAddress {..} = addrHash == addressHash pk

-- | Specialized formatter for 'Address'.
addressF :: Format r (Address -> r)
addressF = build
