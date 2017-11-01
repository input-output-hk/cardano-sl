{- Hierarchical derivation interface.
This module provides basic operations under HD wallets.

* General scheme
    HD wallet has tree-based structure.
    The root of the tree is user's public-secret key pair.
    Each node in the tree corresponds to public-secret key pair.

    Each child node is identified by an index of this child,
    so child's public and secret keys can be _derived_ from parent
    by this index using parent's public-secret key pair.
    On the other hand, a child can't know a parent in reasonable time
    without some extra information.

* Used structure of tree
    In general case a structure of tree can be arbitrary but
    we use specific structure.
    We have 2 layers tree, nodes of the first layer are called "accounts"
    and nodes of the second layer are called "addresses" and are leaves of a tree.
    There can be potentially 2^32 accounts and each account can have 2^32 addresses,
    so wallet can have 2^64 addresses in total.

    The overall scheme is represented on the picture below.

                                 (wallet's public key and secret key)
                                  /             |                  \
                (account[0] (pk, sk))    (account[1] (pk, sk)) ... (account[2^32-1] (pk, sk))
                /                  \                                    \
    (address[0][0] (pk, sk)) ... (address[0][2^32-1] (pk, sk)) ... (address[2^32-1][2^32-1] (pk, sk))

    So each address belongs to one account.
    Addresses are addresses in common sense:
    they can contain money, money can be spent from them,
    address can be used as change address and so on.

    Accounts also reflect an intuition behind the word "account":
    balance of each account can be computed as sum of balances of addresses which belong to it,
    you can have different accounts for different purposes and so on.

* Derivation process
    To derive secret key, parent secret key and passphrase are necessary.
    @deriveHDSecretKey@ does it.
    Description of underlying cryptography you can get here
    https://github.com/bitcoin/bips/blob/master/bip-0032.mediawiki#private-parent-key--private-child-key

    The deriving of public key can be performed using just a public key of a parent node.
    @deriveHDPublicKey@ does it, but we don't use it yet.
    Description of underlying cryptography you can get here
    https://github.com/bitcoin/bips/blob/master/bip-0032.mediawiki#public-parent-key--public-child-key

    It's not allowed to derive a public key from parent public key for
    indicies which are greater than or equal to 2^31.
    For these indicies secret key is required.
    Such way to derive public keys is called "hardened derivation":
    We also don't use it yet.
    See more here
    https://github.com/bitcoin/bips/blob/master/bip-0032.mediawiki#private-parent-key--public-child-key

    Note: all these functions don't require number of derivated level,
    only credential information (public key or secret key with passphrase)
    and index of a child.
    Also we can derive public and secret keys for any arbitrary number of levels.

* HD address payload
    There were mentioned "account" and "address" entities,
    but only addresses appear in the blockchain.
    When we create a transaction, for each TxOut we specify address and coins.
    So neither transaction nor blocks know anything about accounts and wallets.

    We want to be able to track our addresses in the blockchain
    to compute balance of wallet and accounts,
    but as said in _General scheme_ section we aren't able
    to determine a parent without any additional info.
    So we can't determine account and wallet which address belongs to.

    For each leaf let's store path from the root to this leaf along with an address.
    So path is a list of indexes of derivation on each level.
    To hide path from other parties we will encrypt it.
    This encrypted derivation path is called @HDAddressPayload@
    and stored in attributes of Address datatype.

    Note: length of derivation path in our case is 2: @[account index, address index]@.

* Encryption of payload
    To encrypt derivation path let's use AEAD scheme using ChaCha20 and Poly1305.
    To see more information go to https://tools.ietf.org/html/rfc7539.
    Hash of root public key is used as a symmetric key for ChaChaPoly1305.
    This hash called @HDPassphrase@ and function @deriveHDPassphrase@ generates
    it by root public key.

    Encryption of payload performs @packHDAddressAttr@ which takes @HDPassphrase@ and
    derivation path and returns @HDAddressPayload@.
    It serializes derivation path using @Bi@ instance for it and then encrypts produced bytes sequence.

* Decryption of payload
    To decrypt encrypted derivation path we have to derive @HDPassphrase@ again
    from root public key and then try to decrypt @HDAddressPayload@.
    If it's successfully decrypted then it implies address belongs to our tree
    and, vice versa, address doesn't belong to our tree if decryption is failed.

    Note: publishing of root public key gives other parties opportunity to reveal
    all your addresses in the blockchain.
    So it's not safe for the user to share his root public key (like he can do
    it with a usual public key) because he loses his anonymity.

* Recovery process
    So if we have a root secret key (or even root public key),
    we can iterate over the whole blockchain and try to decrypt all met
    addresses to determine which of them belong to our wallet.
    We retrieve addresses along with whole hierarhy of tree because
    decrypted derivation paths describe it.

    Note: we are interested only in addresses mentioned in blockchain,
    and we consider not mentioned addresses having zero balance.
    So we don't store not mentioned addresses anywhere.

* Transaction creation
    To spend @TxIn@ wallet application must sign them using secret key and provide
    public key as a witness.
    For HD wallets we do the same thing: we derive secret and public key of
    address corresponding to @TxIn@ from root secret key using user's passphrase
    and sign @TxIn@ by derived secret key and put public key as a witness.

    Note: although we can reveal all addresses just knowing root public key,
    to spend @TxIn@s root secret key and user's passphrase are needed.
-}

module Pos.Crypto.HD
       ( HDPassphrase (..)
       , HDAddressPayload (..)
       , ShouldCheckPassphrase (..)
       , packHDAddressAttr
       , unpackHDAddressAttr
       , deriveHDPublicKey
       , deriveHDSecretKey
       , deriveHDPassphrase
       , decryptChaChaPoly
       , encryptChaChaPoly
       , toEither

       , firstHardened
       , firstNonHardened
       , isHardened
       ) where

import           Cardano.Crypto.Wallet        (deriveXPrv, deriveXPub, unXPub)
import qualified Crypto.Cipher.ChaChaPoly1305 as C
import           Crypto.Error
import           Crypto.Hash                  (SHA512 (..))
import qualified Crypto.KDF.PBKDF2            as PBKDF2
import qualified Crypto.MAC.Poly1305          as Poly
import           Data.ByteArray               as BA (convert)
import           Data.ByteString.Char8        as B
import           Universum

import           Pos.Binary.Class             (Bi, decodeFull, serialize')
import           Pos.Crypto.Scrypt            (EncryptedPass)
import           Pos.Crypto.Signing.Types     (EncryptedSecretKey (..), PassPhrase,
                                               PublicKey (..), checkPassMatches)

-- | Passphrase is a hash of root public key.
data HDPassphrase = HDPassphrase !ByteString
    deriving Show

-- | HDAddressPayload consists of
--
-- * serialiazed and encrypted using HDPassphrase derivation path from the
-- root key to given descendant key (using ChaChaPoly1305 algorithm)
--
-- * cryptographic tag
--
-- For more information see 'packHDAddressAttr' and 'encryptChaChaPoly'.
data HDAddressPayload
    = HDAddressPayload
    { getHDAddressPayload :: !ByteString
    } deriving (Eq, Ord, Show, Generic)

instance NFData HDAddressPayload

-- | Compute passphrase as hash of the root public key.
deriveHDPassphrase :: PublicKey -> HDPassphrase
deriveHDPassphrase (PublicKey pk) = HDPassphrase $
    PBKDF2.generate
        (PBKDF2.prfHMAC SHA512)
        (PBKDF2.Parameters
             500 -- Parameters for the hashing function. 500 iter of PBDKF2 with HMAC-SHA256
             passLen)
        (unXPub pk)
        ("address-hashing"::ByteString)
  where
    -- Password length in bytes
    passLen = 32

-- Direct children of node are numbered from 0 to 2^32-1.
-- Children with indices less than @firstHardened@ are non-hardened children.
firstHardened :: Word32
firstHardened = 2 ^ (31 :: Word32)

firstNonHardened :: Word32
firstNonHardened = 0

isHardened :: Word32 -> Bool
isHardened = ( >= firstHardened)

-- | Derive public key from public key in non-hardened (normal) way.
-- If you try to pass an 'isHardened' index, error will be called.
deriveHDPublicKey :: PublicKey -> Word32 -> PublicKey
deriveHDPublicKey (PublicKey xpub) childIndex
    | isHardened childIndex =
        error "Wrong index for non-hardened derivation"
    | otherwise =
        maybe (error "deriveHDPublicKey: deriveXPub failed") PublicKey $
          deriveXPub xpub childIndex

-- | Whether to call @checkPassMatches@
newtype ShouldCheckPassphrase = ShouldCheckPassphrase Bool

-- | Derive child's secret key from parent's secret key using user's passphrase.
deriveHDSecretKey
    :: (Bi PassPhrase, Bi EncryptedPass)
    => ShouldCheckPassphrase
    -> PassPhrase
    -> EncryptedSecretKey
    -> Word32
    -> Maybe EncryptedSecretKey
deriveHDSecretKey (ShouldCheckPassphrase checkPass) passPhrase encSK@(EncryptedSecretKey xprv pph) childIndex = do
    when checkPass $ checkPassMatches passPhrase encSK
    pure $
        EncryptedSecretKey
            (deriveXPrv passPhrase xprv childIndex)
            pph

addrAttrNonce :: ByteString
addrAttrNonce = "serokellfore"

-- | Serialize tree path and encrypt it using HDPassphrase via ChaChaPoly1305.
packHDAddressAttr :: HDPassphrase -> [Word32] -> HDAddressPayload
packHDAddressAttr (HDPassphrase passphrase) path = do
    let !pathSer = serialize' path
    let !packCF =
          encryptChaChaPoly
              addrAttrNonce
              passphrase
              ""
              pathSer
    case packCF of
        CryptoFailed er -> error $ "Error in packHDAddressAttr: " <> show er
        CryptoPassed p  -> HDAddressPayload p

-- | Try to decrypt HDAddressPayload using HDPassphrase.
unpackHDAddressAttr :: MonadFail m => HDPassphrase -> HDAddressPayload -> m [Word32]
unpackHDAddressAttr (HDPassphrase passphrase) (HDAddressPayload payload) = do
    let !unpackCF =
          decryptChaChaPoly
              addrAttrNonce
              passphrase
              ""
              payload
    case unpackCF of
        Left er ->
            fail $ "Error in unpackHDAddressAttr, during decryption: " <> show er
        Right p -> case decodeFull p of
            Left er ->
                fail $ "Error in unpackHDAddressAttr, during deserialization: " <> show er
            Right path -> pure path

-- | Take HDPassphrase as symmetric key and serialized derivation path
-- and encrypt it using ChaChaPoly1305 scheme.
encryptChaChaPoly
    :: ByteString -- Nonce (12 random bytes)
    -> ByteString -- Symmetric key (must be 32 bytes)
    -> ByteString -- Encryption header.
                  -- Header is chunk of data we want to transfer unecncrypted
                  -- but still want it to be part of tag digest.
                  -- So tag verifies validity of both encrypted data and unencrypted header.
    -> ByteString -- Input plaintext to be encrypted
    -> CryptoFailable ByteString -- Ciphertext with appended a 128-bit crypto-tag.
encryptChaChaPoly nonce key header plaintext = do
    st1 <- C.nonce12 nonce >>= C.initialize key
    let st2 = C.finalizeAAD $ C.appendAAD header st1
    let (out, st3) = C.encrypt plaintext st2
    let auth = C.finalize st3
    pure $ out <> BA.convert auth

toEither :: CryptoFailable a -> Either Text a
toEither (CryptoPassed x)  = pure x
toEither (CryptoFailed er) = Left $ show er

-- | Take HDPassphrase as symmetric key and encrypted derivation path (aka HDPayload)
-- and try to decrypt it.
-- Verify that appended crypto-tag is the same as gotten in result of work ChaChaPoly1305.
decryptChaChaPoly
    :: ByteString -- Nonce (12 random bytes)
    -> ByteString -- Symmetric key
    -> ByteString -- Encryption header, optional associated data.
    -> ByteString -- Input plaintext to be decrypted
    -> Either Text ByteString -- Decrypted text or error
decryptChaChaPoly nonce key header encDataWithTag = do
    let tagSize = 16::Int
    let l = B.length encDataWithTag
    unless (l >= tagSize) $
        Left $ "Length of encrypted text must be at least " <> show tagSize
    let (encData, rawTag) = B.splitAt (l - 16) encDataWithTag
    tag <- toEither (Poly.authTag rawTag)
    st1 <- toEither (C.nonce12 nonce >>= C.initialize key)
    let st2 = C.finalizeAAD $ C.appendAAD header st1
    let (out, st3) = C.decrypt encData st2
    unless (C.finalize st3 == tag) $
        Left $ "Crypto-tag mismatch"
    -- is it free from mem leaks?
    pure out
