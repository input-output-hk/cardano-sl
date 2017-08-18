-- | Functionality related to 'Address' data type and related types.

module Pos.Core.Address
       (
         -- * Re-exports
         Address (..)
       , StakeholderId

       -- * Formatting
       , addressF
       , addressDetailedF
       , decodeTextAddress

       -- * Spending data checks
       , checkAddrSpendingData
       , checkPubKeyAddress
       , checkScriptAddress
       , checkRedeemAddress

       -- * Utilities
       , addrSpendingDataToType
       , addrAttributesUnwrapped
       , deriveLvl2KeyPair
       , isRedeemAddress
       , isUnknownAddressType

       -- * Construction
       , makeAddress
       , makePubKeyAddress
       , makePubKeyHdwAddress
       , makeScriptAddress
       , makeRedeemAddress

       , createHDAddressNH
       , createHDAddressH

         -- * Internals
       , AddressHash
       , addressHash
       , unsafeAddressHash
       ) where

import           Universum

import           Control.Lens            (_Left)
import           Crypto.Hash             (Blake2b_224, Digest, SHA3_256)
import qualified Crypto.Hash             as CryptoHash
import           Data.ByteString.Base58  (Alphabet (..), bitcoinAlphabet, decodeBase58,
                                          encodeBase58)
import           Data.Hashable           (Hashable (..))
import qualified Data.Text.Buildable     as Buildable
import           Formatting              (Format, bprint, build, builder, int, later, (%))
import           Serokell.Util           (listJson, pairBuilder)

import           Pos.Binary.Class        (Bi)
import qualified Pos.Binary.Class        as Bi
import           Pos.Binary.Core.Address ()
import           Pos.Binary.Crypto       ()
import           Pos.Core.Types          (AddrAttributes (..), AddrSpendingData (..),
                                          AddrStakeDistribution (..), AddrType (..),
                                          Address (..), Address' (..), AddressHash,
                                          Script, StakeholderId)
import           Pos.Crypto              (AbstractHash (AbstractHash), EncryptedSecretKey,
                                          PublicKey, RedeemPublicKey, encToPublic,
                                          hashHexF, shortHashF)
import           Pos.Crypto.HD           (HDAddressPayload, HDPassphrase,
                                          deriveHDPassphrase, deriveHDPublicKey,
                                          deriveHDSecretKey, packHDAddressAttr)
import           Pos.Crypto.SafeSigning  (PassPhrase)
import           Pos.Data.Attributes     (attrData, mkAttributes)

instance Hashable Address where
    hashWithSalt s = hashWithSalt s . Bi.serialize'

----------------------------------------------------------------------------
-- Formatting, pretty-printing
----------------------------------------------------------------------------

instance Buildable AddrSpendingData where
    build =
        \case
            PubKeyASD pk -> bprint ("PubKeyASD " %build) pk
            ScriptASD script -> bprint ("ScriptASD "%build) script
            RedeemASD rpk -> bprint ("RedeemASD "%build) rpk
            UnknownASD tag _ -> bprint ("UnknownASD with tag "%int) tag

instance Buildable AddrStakeDistribution where
    build =
        \case
            BootstrapEraDistr -> "Bootstrap era distribution"
            SingleKeyDistr id ->
                bprint ("Single key distribution ("%shortHashF%")") id
            MultiKeyDistr distr ->
                bprint
                    ("Multi key distribution: "%listJson)
                    (map pairBuilder distr)

instance Buildable AddrAttributes where
    build (AddrAttributes {..}) =
        bprint
            ("AddrAttributes { stake distribution: "%build%
             ", derivation path: "%builder%" }")
            aaStakeDistribution
            derivationPathBuilder
      where
        derivationPathBuilder =
            case aaPkDerivationPath of
                Nothing -> "{}"
                Just _  -> "{path is encrypted}"

-- | A formatter showing guts of an 'Address'.
addressDetailedF :: Format r (Address -> r)
addressDetailedF =
    later $ \Address {..} ->
        bprint (builder%" address with root "%hashHexF%", attributes: "%build)
            (formattedType addrType) addrRoot addrAttributes
  where
    formattedType =
        \case
            ATPubKey      -> "PubKey"
            ATScript      -> "Script"
            ATRedeem      -> "Redeem"
            ATUnknown tag -> "Unknown#" <> Buildable.build tag

-- | Currently we gonna use Bitcoin alphabet for representing addresses in
-- base58
addrAlphabet :: Alphabet
addrAlphabet = bitcoinAlphabet

addrToBase58 :: Address -> ByteString
addrToBase58 = encodeBase58 addrAlphabet . Bi.serialize'

instance Buildable Address where
    build = Buildable.build . decodeUtf8 @Text . addrToBase58

-- | Specialized formatter for 'Address'.
addressF :: Format r (Address -> r)
addressF = build

-- | A function which decodes base58-encoded 'Address'.
decodeTextAddress :: Text -> Either Text Address
decodeTextAddress = first toText . decodeAddress . encodeUtf8
  where
    decodeAddress :: ByteString -> Either String Address
    decodeAddress bs = do
        let base58Err = "Invalid base58 representation of address"
        dbs <- maybeToRight base58Err $ decodeBase58 addrAlphabet bs
        over _Left toString $ Bi.decodeFull dbs

----------------------------------------------------------------------------
-- Constructors
----------------------------------------------------------------------------

-- | Make an 'Address' from spending data and attributes.
makeAddress :: AddrSpendingData -> AddrAttributes -> Address
makeAddress spendingData attributesUnwrapped =
    Address
    { addrRoot = addressHash address'
    , addrAttributes = attributes
    , ..
    }
  where
    addrType = addrSpendingDataToType spendingData
    attributes = mkAttributes attributesUnwrapped
    address' = Address' (addrType, spendingData, attributes)

-- TODO [CSL-1489] Consider stake distribution.

-- | A function for making an address from 'PublicKey'.
makePubKeyAddress :: PublicKey -> Address
makePubKeyAddress = makePubKeyAddressImpl Nothing

-- | A function for making an HDW address
makePubKeyHdwAddress
    :: HDAddressPayload    -- ^ Derivation path
    -> PublicKey
    -> Address
makePubKeyHdwAddress path = makePubKeyAddressImpl (Just path)

makePubKeyAddressImpl :: Maybe HDAddressPayload -> PublicKey -> Address
makePubKeyAddressImpl path key = makeAddress spendingData attrs
  where
    spendingData = PubKeyASD key
    attrs =
        AddrAttributes
        {aaStakeDistribution = BootstrapEraDistr, aaPkDerivationPath = path}

-- | A function for making an address from a validation 'Script'.
makeScriptAddress :: Bi Script => Script -> Address
makeScriptAddress scr = makeAddress spendingData attrs
  where
    spendingData = ScriptASD scr
    attrs =
        AddrAttributes
        {aaStakeDistribution = BootstrapEraDistr, aaPkDerivationPath = Nothing}

-- | A function for making an address from 'RedeemPublicKey'.
makeRedeemAddress :: Bi RedeemPublicKey => RedeemPublicKey -> Address
makeRedeemAddress key = makeAddress spendingData attrs
  where
    spendingData = RedeemASD key
    attrs =
        AddrAttributes
        {aaStakeDistribution = BootstrapEraDistr, aaPkDerivationPath = Nothing}

-- | Create address from secret key in hardened way.
createHDAddressH
    :: PassPhrase
    -> HDPassphrase
    -> EncryptedSecretKey
    -> [Word32]
    -> Word32
    -> Maybe (Address, EncryptedSecretKey)
createHDAddressH passphrase walletPassphrase parent parentPath childIndex = do
    derivedSK <- deriveHDSecretKey passphrase parent childIndex
    let addressPayload = packHDAddressAttr walletPassphrase $ parentPath ++ [childIndex]
    let pk = encToPublic derivedSK
    return (makePubKeyHdwAddress addressPayload pk, derivedSK)

-- | Create address from public key via non-hardened way.
createHDAddressNH :: HDPassphrase -> PublicKey -> [Word32] -> Word32 -> (Address, PublicKey)
createHDAddressNH passphrase parent parentPath childIndex = do
    let derivedPK = deriveHDPublicKey parent childIndex
    let addressPayload = packHDAddressAttr passphrase $ parentPath ++ [childIndex]
    (makePubKeyHdwAddress addressPayload derivedPK, derivedPK)

----------------------------------------------------------------------------
-- Checks
----------------------------------------------------------------------------

-- | Check whether given 'AddrSpendingData' corresponds to given
-- 'Address'.
checkAddrSpendingData :: AddrSpendingData -> Address -> Bool
checkAddrSpendingData asd Address {..} =
    addrRoot == addressHash address' && addrType == addrSpendingDataToType asd
  where
    address' = Address' (addrType, asd, addrAttributes)

-- | Check if given 'Address' is created from given 'PublicKey'
checkPubKeyAddress :: PublicKey -> Address -> Bool
checkPubKeyAddress pk = checkAddrSpendingData (PubKeyASD pk)

-- | Check if given 'Address' is created from given validation script
checkScriptAddress :: Script -> Address -> Bool
checkScriptAddress script = checkAddrSpendingData (ScriptASD script)

-- | Check if given 'Address' is created from given 'RedeemPublicKey'
checkRedeemAddress :: RedeemPublicKey -> Address -> Bool
checkRedeemAddress rpk = checkAddrSpendingData (RedeemASD rpk)

----------------------------------------------------------------------------
-- Hashing
----------------------------------------------------------------------------

unsafeAddressHash :: Bi a => a -> AddressHash b
unsafeAddressHash = AbstractHash . secondHash . firstHash
  where
    firstHash :: Bi a => a -> Digest SHA3_256
    firstHash = CryptoHash.hash . Bi.serialize'
    secondHash :: Digest SHA3_256 -> Digest Blake2b_224
    secondHash = CryptoHash.hash

addressHash :: Bi a => a -> AddressHash a
addressHash = unsafeAddressHash

----------------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------------

-- | Convert 'AddrSpendingData' to the corresponding 'AddrType'.
addrSpendingDataToType :: AddrSpendingData -> AddrType
addrSpendingDataToType =
    \case
        PubKeyASD {} -> ATPubKey
        ScriptASD {} -> ATScript
        RedeemASD {} -> ATRedeem
        UnknownASD tag _ -> ATUnknown tag

-- | Check whether an 'Address' is redeem address.
isRedeemAddress :: Address -> Bool
isRedeemAddress Address {..} =
    case addrType of
        ATRedeem -> True
        _        -> False

isUnknownAddressType :: Address -> Bool
isUnknownAddressType Address {..} =
    case addrType of
        ATUnknown {} -> True
        _            -> False

-- | Get 'AddrAttributes' from 'Address'.
addrAttributesUnwrapped :: Address -> AddrAttributes
addrAttributesUnwrapped = attrData . addrAttributes

-- | Makes account secret key for given wallet set.
deriveLvl2KeyPair
    :: PassPhrase
    -> EncryptedSecretKey  -- ^ key of wallet set
    -> Word32              -- ^ wallet derivation index
    -> Word32              -- ^ account derivation index
    -> Maybe (Address, EncryptedSecretKey)
deriveLvl2KeyPair passphrase wsKey walletIndex accIndex = do
    wKey <- deriveHDSecretKey passphrase wsKey walletIndex
    let hdPass = deriveHDPassphrase $ encToPublic wsKey
    createHDAddressH passphrase hdPass wKey [walletIndex] accIndex
