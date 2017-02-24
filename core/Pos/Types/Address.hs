{-# LANGUAGE UndecidableInstances #-}

module Pos.Types.Address
       ( Address (..)
       , AddrPkAttrs (..)
       , addressF
       , addressDetailedF
       , checkPubKeyAddress
       , checkScriptAddress
       , checkRedeemAddress
       , checkUnknownAddressType
       , makePubKeyAddress
       , makePubKeyHdwAddress
       , makeScriptAddress
       , makeRedeemAddress
       , decodeTextAddress

       , StakeholderId

         -- * Internals
       , AddressHash
       , addressHash
       , unsafeAddressHash
       ) where

import           Crypto.Hash            (Blake2s_224, Digest, SHA3_256, hashlazy)
import qualified Crypto.Hash            as CryptoHash
import           Data.ByteString.Base58 (Alphabet (..), bitcoinAlphabet, decodeBase58,
                                         encodeBase58)
import qualified Data.ByteString.Lazy   as BSL (fromStrict, toStrict)
import           Data.Hashable          (Hashable (..))
import           Data.Text.Buildable    (Buildable)
import qualified Data.Text.Buildable    as Buildable
import           Formatting             (Format, bprint, build, later, (%))
import           Serokell.Util.Base16   (base16F)
import           Serokell.Util.Text     (listJson)
import           Universum

import           Pos.Binary.Class       (Bi)
import qualified Pos.Binary.Class       as Bi
import           Pos.Crypto             (AbstractHash (AbstractHash), PublicKey, RedeemPublicKey)
import           Pos.Data.Attributes    (mkAttributes)
import           Pos.Types.Core         (AddrPkAttrs (..), Address (..), AddressHash,
                                         StakeholderId)
import           Pos.Types.Script       (Script)

instance Bi Address => Hashable Address where
    hashWithSalt s = hashWithSalt s . Bi.encode

-- | Currently we gonna use Bitcoin alphabet for representing addresses in
-- base58
addrAlphabet :: Alphabet
addrAlphabet = bitcoinAlphabet

addrToBase58 :: Bi Address => Address -> ByteString
addrToBase58 = encodeBase58 addrAlphabet . BSL.toStrict . Bi.encode

instance Bi Address => Buildable Address where
    build = Buildable.build . decodeUtf8 @Text . addrToBase58

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
    => PublicKey
    -> [Word32]         -- ^ Derivation path
    -> Address
makePubKeyHdwAddress key path =
    PubKeyAddress (addressHash key)
                  (mkAttributes (AddrPkAttrs (Just path)))

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
        Nothing   -> "{}"
        Just path -> bprint ("{path: "%listJson%"}") path

-- | A formatter showing guts of an 'Address'.
addressDetailedF :: Format r (Address -> r)
addressDetailedF = later $ \case
    PubKeyAddress x attrs ->
        bprint ("PubKeyAddress "%build%" (attrs: "%build%")") x attrs
    ScriptAddress x ->
        bprint ("ScriptAddress "%build) x
    RedeemAddress x ->
        bprint ("RedeemAddress "%build) x
    UnknownAddressType t bs ->
        bprint ("UnknownAddressType "%build%" "%base16F) t bs

----------------------------------------------------------------------------
-- Hashing
----------------------------------------------------------------------------

unsafeAddressHash :: Bi a => a -> AddressHash b
unsafeAddressHash = AbstractHash . secondHash . firstHash
  where
    firstHash :: Bi a => a -> Digest SHA3_256
    firstHash = hashlazy . Bi.encode
    secondHash :: Digest SHA3_256 -> Digest Blake2s_224
    secondHash = CryptoHash.hash

addressHash :: Bi a => a -> AddressHash a
addressHash = unsafeAddressHash
