module Pos.Core.Address
       ( Address (..)
       , AddrPkAttrs (..)
       , AddressIgnoringAttributes (..)
       , addressF
       , addressDetailedF
       , checkPubKeyAddress
       , checkScriptAddress
       , checkRedeemAddress
       , checkUnknownAddressType
       , makePubKeyAddress
       , makePubKeyHdwAddress
       , createHDAddressNH
       , createHDAddressH
       , makeScriptAddress
       , makeRedeemAddress
       , decodeTextAddress

       , StakeholderId

         -- * Internals
       , AddressHash
       , addressHash
       , unsafeAddressHash
       ) where

import           Crypto.Hash            (Blake2b_224, Digest, SHA3_256, hashlazy)
import qualified Crypto.Hash            as CryptoHash
import           Data.ByteString.Base58 (Alphabet (..), bitcoinAlphabet, decodeBase58,
                                         encodeBase58)
import qualified Data.ByteString.Lazy   as BSL (fromStrict)
import           Data.Hashable          (Hashable (..))
import           Data.Text.Buildable    (Buildable)
import qualified Data.Text.Buildable    as Buildable
import           Formatting             (Format, bprint, build, int, later, (%))
import           Serokell.Util.Base16   (base16F)
import           Universum

import           Pos.Binary.Class       (Bi)
import qualified Pos.Binary.Class       as Bi
import           Pos.Binary.Crypto      ()
import           Pos.Core.Types         (AddrPkAttrs (..), Address (..), AddressHash,
                                         Script, StakeholderId)
import           Pos.Crypto             (AbstractHash (AbstractHash), EncryptedSecretKey,
                                         PublicKey, RedeemPublicKey, encToPublic,
                                         hashHexF)
import           Pos.Crypto.HD          (HDAddressPayload, HDPassphrase,
                                         deriveHDPublicKey, deriveHDSecretKey,
                                         packHDAddressAttr)
import           Pos.Crypto.SafeSigning (PassPhrase)
import           Pos.Data.Attributes    (mkAttributes)

instance Bi Address => Hashable Address where
    hashWithSalt s = hashWithSalt s . Bi.encode

-- | Currently we gonna use Bitcoin alphabet for representing addresses in
-- base58
addrAlphabet :: Alphabet
addrAlphabet = bitcoinAlphabet

addrToBase58 :: Bi Address => Address -> ByteString
addrToBase58 = encodeBase58 addrAlphabet . Bi.encodeStrict

instance Bi Address => Buildable Address where
    build = Buildable.build . decodeUtf8 @Text . addrToBase58

newtype AddressIgnoringAttributes = AddressIA Address

instance Eq AddressIgnoringAttributes where
    AddressIA (PubKeyAddress h1 _) == AddressIA (PubKeyAddress h2 _) = h1 == h2
    AddressIA a1                   == AddressIA a2                   = a1 == a2

instance Ord AddressIgnoringAttributes where
    AddressIA (PubKeyAddress h1 _) `compare` AddressIA (PubKeyAddress h2 _) =
        h1 `compare` h2
    AddressIA a1 `compare` AddressIA a2 = compare a1 a2

instance Bi Address => Hashable AddressIgnoringAttributes where
    hashWithSalt s (AddressIA (PubKeyAddress h _)) = hashWithSalt s h
    hashWithSalt s (AddressIA a)                   = hashWithSalt s a

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
makePubKeyAddress :: Bi PublicKey => PublicKey -> Address
makePubKeyAddress key =
    PubKeyAddress (addressHash key)
                  (mkAttributes (AddrPkAttrs Nothing))

-- | A function for making an HDW address
makePubKeyHdwAddress
    :: Bi PublicKey
    => HDAddressPayload    -- ^ Derivation path
    -> PublicKey
    -> Address
makePubKeyHdwAddress path key =
    PubKeyAddress (addressHash key)
                  (mkAttributes (AddrPkAttrs (Just path)))

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

-- | A function for making an address from a validation script
makeScriptAddress :: Bi Script => Script -> Address
makeScriptAddress scr = ScriptAddress (addressHash scr)

-- | A function for making an address from a redeem script
makeRedeemAddress :: Bi RedeemPublicKey => RedeemPublicKey -> Address
makeRedeemAddress key = RedeemAddress (addressHash key)

-- CHECK: @checkPubKeyAddress
-- | Check if given 'Address' is created from given 'PublicKey'
checkPubKeyAddress :: Bi PublicKey => PublicKey -> Address -> Bool
checkPubKeyAddress key (PubKeyAddress h _) = addressHash key == h
checkPubKeyAddress _ _                     = False

-- | Check if given 'Address' is created from given validation script
checkScriptAddress :: Bi Script => Script -> Address -> Bool
checkScriptAddress scr (ScriptAddress h) = addressHash scr == h
checkScriptAddress _ _                   = False

-- | Check if given 'Address' is created from given 'RedeemPublicKey'
checkRedeemAddress :: Bi RedeemPublicKey => RedeemPublicKey -> Address -> Bool
checkRedeemAddress key (RedeemAddress h) = addressHash key == h
checkRedeemAddress _ _                   = False

-- | Check if given 'Address' has given type
checkUnknownAddressType :: Word8 -> Address -> Bool
checkUnknownAddressType t addr = case addr of
    PubKeyAddress{}        -> t == 0
    ScriptAddress{}        -> t == 1
    RedeemAddress{}        -> t == 2
    UnknownAddressType p _ -> t == p

-- | Specialized formatter for 'Address'.
addressF :: Bi Address => Format r (Address -> r)
addressF = build

instance Buildable AddrPkAttrs where
    build (AddrPkAttrs p) = case p of
        Nothing -> "{}"
        Just _  -> bprint ("{path is encrypted}")

-- | A formatter showing guts of an 'Address'.
addressDetailedF :: Format r (Address -> r)
addressDetailedF = later $ \case
    PubKeyAddress x attrs ->
        bprint ("PubKeyAddress "%hashHexF%" (attrs: "%build%")") x attrs
    ScriptAddress x ->
        bprint ("ScriptAddress "%hashHexF) x
    RedeemAddress x ->
        bprint ("RedeemAddress "%hashHexF) x
    UnknownAddressType t bs ->
        bprint ("UnknownAddressType "%int%" "%base16F) t bs

----------------------------------------------------------------------------
-- Hashing
----------------------------------------------------------------------------

unsafeAddressHash :: Bi a => a -> AddressHash b
unsafeAddressHash = AbstractHash . secondHash . firstHash
  where
    firstHash :: Bi a => a -> Digest SHA3_256
    firstHash = hashlazy . Bi.encode
    secondHash :: Digest SHA3_256 -> Digest Blake2b_224
    secondHash = CryptoHash.hash

addressHash :: Bi a => a -> AddressHash a
addressHash = unsafeAddressHash
