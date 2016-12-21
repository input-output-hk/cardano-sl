{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}

module Pos.Types.Address
       ( AddressVersion (..)
       , AddressDestination (..)
       , Address (..)
       , addressF
       , addressDetailedF
       , checkPubKeyAddress
       , checkScriptAddress
       , makePubKeyAddress
       , makeScriptAddress
       , decodeTextAddress

         -- * Internals
       , AddressHash
       , addressHash
       , unsafeAddressHash
       , curPubKeyAddrVersion
       , curScriptAddrVersion
       ) where

import           Control.Lens           (view, _3)
import           Crypto.Hash            (Blake2s_224, Digest, SHA3_256, hashlazy)
import qualified Crypto.Hash            as CryptoHash
import           Data.Bits              ((.|.))
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
import           Formatting             (Format, bprint, build, int, later, shown, (%))
import           Prelude                (String, readsPrec, show)
import           Universum              hiding (show)

import           Pos.Binary.Class       (Bi)
import qualified Pos.Binary.Class       as Bi
import           Pos.Binary.Crypto      ()
import           Pos.Crypto             (AbstractHash (AbstractHash), PublicKey)
import           Pos.Script             (Script)

-- | Address versions are here for dealing with possible backwards
-- compatibility issues in the future
newtype AddressVersion = AddressVersion {unwrapAddressVersion :: Word8}
  deriving (Eq, Generic, Ord)

curPubKeyAddrVersion :: AddressVersion
curPubKeyAddrVersion = AddressVersion 0       -- encoded as “0”

curScriptAddrVersion :: AddressVersion
curScriptAddrVersion = AddressVersion 0       -- encoded as “128”

-- | Address is where you can send coins.
data Address = Address
    { addrVersion      :: !AddressVersion
    , addrDestination  :: !AddressDestination
    , addrDistribution :: !(Maybe [(PublicKey, Int)])
    } deriving (Eq, Generic, Ord)

data AddressDestination
    = PubKeyDestination { addrDestKeyHash :: !(AddressHash PublicKey)}
    | ScriptDestination { addrDestScriptHash :: !(AddressHash Script)}
    deriving (Eq, Generic, Ord)

instance CRC32 Address where
    crc32Update seed Address{..} =
        seed & flip crc32Update [ver]
             & flip crc32Update dest
             & flip crc32Update (Bi.encode addrDistribution)
      where
        ver = case addrDestination of
            PubKeyDestination{} -> unwrapAddressVersion addrVersion
            ScriptDestination{} -> unwrapAddressVersion addrVersion .|. 128
        dest = case addrDestination of
            PubKeyDestination x -> Bi.encode x
            ScriptDestination x -> Bi.encode x

instance Bi Address => Hashable Address where
    hashWithSalt s = hashWithSalt s . Bi.encode

-- | Currently we gonna use Bitcoin alphabet for representing addresses in base58
addrAlphabet :: Alphabet
addrAlphabet = bitcoinAlphabet

addrToBase58 :: Bi Address => Address -> ByteString
addrToBase58 = encodeBase58 addrAlphabet . BSL.toStrict . Bi.encode

instance Bi Address => Show Address where
    show = BSC.unpack . addrToBase58

instance Bi Address => Buildable Address where
    build = Buildable.build . decodeUtf8 @Text . addrToBase58

instance NFData AddressVersion
instance NFData AddressDestination
instance NFData Address

instance Bi Address => Read Address where
    readsPrec _ str =
        let trimmedStr = dropWhile isSpace str
            (addrStr, rest) = span (`BSC.elem` unAlphabet addrAlphabet) trimmedStr
            eAddr = decodeAddress $ encodeUtf8 addrStr
        in case eAddr of
               Left _     -> []
               Right addr -> [(addr, rest)]

-- | A function which decodes base58 address from given ByteString
decodeAddress :: Bi Address => ByteString -> Either String Address
decodeAddress bs = do
    let base58Err = "Invalid base58 representation of address"
        takeErr = toString . view _3
        takeRes = view _3
    dbs <- maybeToRight base58Err $ decodeBase58 addrAlphabet bs
    bimap takeErr takeRes $ Bi.decodeOrFail $ BSL.fromStrict dbs

decodeTextAddress :: Bi Address => Text -> Either Text Address
decodeTextAddress = first toText . decodeAddress . encodeUtf8

-- | A function for making an address from PublicKey
makePubKeyAddress :: PublicKey -> Address
makePubKeyAddress key = Address {
    addrVersion = curPubKeyAddrVersion,
    addrDestination = PubKeyDestination (addressHash key),
    addrDistribution = Nothing }

-- | A function for making an address from a validation script
makeScriptAddress :: Bi Script => Script -> Address
makeScriptAddress scr = Address {
    addrVersion = curScriptAddrVersion,
    addrDestination = ScriptDestination (addressHash scr),
    addrDistribution = Nothing }

-- | Check if given 'Address' is created from given 'PublicKey'
checkPubKeyAddress :: PublicKey -> Address -> Bool
checkPubKeyAddress key Address{..} =
    case addrDestination of
        PubKeyDestination x -> x == addressHash key
        _                   -> False

-- | Check if given 'Address' is created from given validation script
checkScriptAddress :: Bi Script => Script -> Address -> Bool
checkScriptAddress scr Address{..} =
    case addrDestination of
        ScriptDestination x -> x == addressHash scr
        _                   -> False

-- | Specialized formatter for 'Address'.
addressF :: Bi Address => Format r (Address -> r)
addressF = build

-- | A formatter showing guts of an 'Address'.
addressDetailedF :: Format r (Address -> r)
addressDetailedF = later $ \Address {..} ->
    let ver = unwrapAddressVersion addrVersion
    in bprint
           ("Address dest = " %build%", "%build)
           (case addrDestination of
                PubKeyDestination x ->
                    bprint ("pubkey(v"%int%"):"%build) ver x
                ScriptDestination x ->
                    bprint ("script(v"%int%"):"%build) ver x)
           (case addrDistribution of
                Nothing -> "no distribution"
                Just d  -> bprint ("distr = "%shown) d)

----------------------------------------------------------------------------
-- Hashing
----------------------------------------------------------------------------

type AddressHash = AbstractHash Blake2s_224

unsafeAddressHash :: Bi a => a -> AddressHash b
unsafeAddressHash = AbstractHash . secondHash . firstHash
  where
    firstHash :: Bi a => a -> Digest SHA3_256
    firstHash = hashlazy . Bi.encode
    secondHash :: Digest SHA3_256 -> Digest Blake2s_224
    secondHash = CryptoHash.hash

addressHash :: Bi a => a -> AddressHash a
addressHash = unsafeAddressHash
