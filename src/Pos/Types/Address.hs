{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}

module Pos.Types.Address
       ( Address (..)
       , addressF
       , checkPubKeyAddress
       , checkScriptAddress
       , makePubKeyAddress
       , makeScriptAddress
       , decodeTextAddress

         -- * Internals
       , AddressHash
       , addressHash
       , unsafeAddressHash
       , curAddrVersion
       ) where

import           Control.Lens           (view, _3)
import           Crypto.Hash            (Blake2s_224, Digest, SHA3_256, hashlazy)
import qualified Crypto.Hash            as CryptoHash
import           Data.Aeson             (ToJSON (toJSON))
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
import           Formatting             (Format, build, sformat)
import           Prelude                (String, readsPrec, show)
import           Universum              hiding (show)

import           Pos.Binary.Class       (Bi)
import qualified Pos.Binary.Class       as Bi
import           Pos.Binary.Crypto      ()
import           Pos.Crypto             (AbstractHash (AbstractHash), PublicKey)
import           Pos.Script             (Script)

-- | Address versions are here for dealing with possible backwards
-- compatibility issues in the future
-- For P2PKH addresses, we use versions from 0 to 127
-- For P2SH addresses, we use versions from 128 to 255
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

instance Bi (AddressHash PublicKey) => CRC32 Address where
    crc32Update seed PubKeyAddress {..} =
        crc32Update (crc32Update seed [addrVersion]) $
        Bi.encode addrKeyHash
    crc32Update seed ScriptAddress {..} =
        crc32Update (crc32Update seed [addrVersion .|. 128]) $
        Bi.encode addrScriptHash

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

instance NFData Address

instance Bi Address => ToJSON Address where
    toJSON = toJSON . sformat build

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
makePubKeyAddress = PubKeyAddress curAddrVersion . addressHash

-- | A function for making an address from a validation script
makeScriptAddress :: Bi Script => Script -> Address
makeScriptAddress = ScriptAddress (curAddrVersion .|. 128) . addressHash

-- | Check if given 'Address' is created from given 'PublicKey'
checkPubKeyAddress :: PublicKey -> Address -> Bool
checkPubKeyAddress pk PubKeyAddress{..} = addrKeyHash == addressHash pk
checkPubKeyAddress _ _                  = False

-- | Check if given 'Address' is created from given validation script
checkScriptAddress :: Bi Script => Script -> Address -> Bool
checkScriptAddress scr ScriptAddress{..} = addrScriptHash == addressHash scr
checkScriptAddress _ _                   = False

-- | Specialized formatter for 'Address'.
addressF :: Bi Address => Format r (Address -> r)
addressF = build

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
