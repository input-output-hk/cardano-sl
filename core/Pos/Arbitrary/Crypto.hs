-- | `Arbitrary` instances for using in tests and benchmarks

module Pos.Arbitrary.Crypto
       ( SharedSecrets (..)
       ) where

import           Universum

import           Control.Monad                     (zipWithM)
import qualified Data.ByteArray                    as ByteArray
import           Data.List.NonEmpty                (fromList)
import           Test.QuickCheck                   (Arbitrary (..), choose, elements,
                                                    oneof, vector)
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary, genericShrink)

import           Pos.Arbitrary.Crypto.Unsafe       ()
import           Pos.Binary.Class                  (AsBinary (..), AsBinaryClass (..), Bi)
import           Pos.Binary.Crypto                 ()
import           Pos.Crypto.AsBinary               ()
import           Pos.Crypto.Hashing                (AbstractHash, HashAlgorithm)
import           Pos.Crypto.HD                     (HDAddressPayload, HDPassphrase (..))
import           Pos.Crypto.RedeemSigning          (RedeemPublicKey, RedeemSecretKey,
                                                    RedeemSignature,
                                                    redeemDeterministicKeyGen, redeemSign)
import           Pos.Crypto.SafeSigning            (PassPhrase, createProxyCert,
                                                    createPsk)
import           Pos.Crypto.SecretSharing          (EncShare, Secret, SecretProof,
                                                    SecretSharingExtra, Share, Threshold,
                                                    VssKeyPair, VssPublicKey,
                                                    decryptShare, deterministicVssKeyGen,
                                                    genSharedSecret, toVssPublicKey)
import           Pos.Crypto.Signing                (ProxyCert, ProxySecretKey,
                                                    ProxySignature, PublicKey, SecretKey,
                                                    Signature, Signed,
                                                    deterministicKeyGen, mkSigned,
                                                    proxySign, sign, toPublic)
import           Pos.Crypto.SignTag                (SignTag (..))
import           Pos.Util.Arbitrary                (arbitrarySizedS, arbitraryUnsafe)

----------------------------------------------------------------------------
-- SignTag
----------------------------------------------------------------------------

instance Arbitrary SignTag where
    arbitrary = genericArbitrary
    shrink = genericShrink

----------------------------------------------------------------------------
-- Arbitrary signing keys
----------------------------------------------------------------------------

instance Arbitrary PublicKey where
    arbitrary = fst . deterministicKeyGen <$> arbitrarySizedS 32
instance Arbitrary SecretKey where
    arbitrary = snd . deterministicKeyGen <$> arbitrarySizedS 32

instance Arbitrary RedeemPublicKey where
    arbitrary = fst . fromMaybe err . redeemDeterministicKeyGen <$>
                arbitrarySizedS 32
      where err = error "arbitrary@RedeemPublicKey: keygen failed"
instance Arbitrary RedeemSecretKey where
    arbitrary = snd . fromMaybe err . redeemDeterministicKeyGen <$>
                arbitrarySizedS 32
      where err = error "arbitrary@RedeemSecretKey: keygen failed"

----------------------------------------------------------------------------
-- Arbitrary VSS keys
----------------------------------------------------------------------------

instance Arbitrary VssKeyPair where
    arbitrary = deterministicVssKeyGen <$> arbitrarySizedS 40

instance Arbitrary VssPublicKey where
    arbitrary = toVssPublicKey <$> arbitrary

instance Arbitrary (AsBinary VssPublicKey) where
    arbitrary = asBinary @VssPublicKey <$> arbitrary

----------------------------------------------------------------------------
-- Arbitrary signatures
----------------------------------------------------------------------------

instance (Bi a, Arbitrary a) => Arbitrary (Signature a) where
    arbitrary = sign <$> arbitrary <*> arbitrary <*> arbitrary

instance (Bi a, Arbitrary a) => Arbitrary (RedeemSignature a) where
    arbitrary = redeemSign <$> arbitrary <*> arbitrary

instance (Bi a, Arbitrary a) => Arbitrary (Signed a) where
    arbitrary = mkSigned <$> arbitrary <*> arbitrary <*> arbitrary

instance (Bi w, Arbitrary w) => Arbitrary (ProxyCert w) where
    arbitrary = liftA3 createProxyCert arbitrary arbitrary arbitrary

instance (Bi w, Arbitrary w) => Arbitrary (ProxySecretKey w) where
    arbitrary = liftA3 createPsk arbitrary arbitrary arbitrary

instance (Bi w, Arbitrary w, Bi a, Arbitrary a) =>
         Arbitrary (ProxySignature w a) where
    arbitrary = do
        delegateSk <- arbitrary
        issuerSk <- arbitrary
        w <- arbitrary
        let psk = createPsk issuerSk (toPublic delegateSk) w
        proxySign SignProxySK delegateSk psk <$> arbitrary

----------------------------------------------------------------------------
-- Arbitrary secrets
----------------------------------------------------------------------------

data SharedSecrets = SharedSecrets
    { ssSecShare  :: SecretSharingExtra
    , ssSecret    :: Secret
    , ssSecProof  :: SecretProof
    , ssShares    :: [(EncShare, Share)]
    , ssThreshold :: Threshold
    , ssVSSPKs    :: [VssPublicKey]
    , ssPos       :: Int            -- This field is a valid, zero-based index in the
                                    -- shares/keys lists.
    } deriving (Show, Eq)

instance Arbitrary SharedSecrets where
    arbitrary = do
        parties <- choose (6, 50)
        threshold <- choose (3, toInteger parties - 2)
        vssKeys <- vector parties
        (ss, s, sp, encryptedShares) <-
            genSharedSecret threshold (map toVssPublicKey $ fromList vssKeys)
        decryptedShares <- zipWithM decryptShare vssKeys encryptedShares
        let shares = zip encryptedShares decryptedShares
            vssPKs = map toVssPublicKey vssKeys
        pure $ SharedSecrets ss s sp shares threshold vssPKs (parties - 1)

instance Arbitrary SecretSharingExtra where
    arbitrary = ssSecShare <$> arbitrary

instance Arbitrary (AsBinary SecretSharingExtra) where
    arbitrary = asBinary @SecretSharingExtra <$> arbitrary

instance Arbitrary (AsBinary SecretProof) where
    arbitrary = asBinary @SecretProof <$> arbitrary

instance Arbitrary Secret where
    arbitrary = ssSecret <$> arbitrary

instance Arbitrary (AsBinary Secret) where
    arbitrary = asBinary @Secret <$> arbitrary

instance Arbitrary SecretProof where
    arbitrary = ssSecProof <$> arbitrary

instance Arbitrary EncShare where
    arbitrary = elements . map fst . ssShares =<< arbitrary

instance Arbitrary (AsBinary EncShare) where
    arbitrary = asBinary @EncShare <$> arbitrary

instance Arbitrary Share where
    arbitrary = elements . map snd . ssShares =<< arbitrary

instance Arbitrary (AsBinary Share) where
    arbitrary = asBinary @Share <$> arbitrary

----------------------------------------------------------------------------
-- Arbitrary hashes
----------------------------------------------------------------------------

instance (HashAlgorithm algo, Bi a) => Arbitrary (AbstractHash algo a) where
    arbitrary = arbitraryUnsafe

----------------------------------------------------------------------------
-- Arbitrary passphrases
----------------------------------------------------------------------------

instance Arbitrary PassPhrase where
    arbitrary = oneof [
        pure mempty,
        ByteArray.pack <$> vector 32
        ]

----------------------------------------------------------------------------
-- HD
----------------------------------------------------------------------------

instance Arbitrary HDPassphrase where
    arbitrary = HDPassphrase <$> arbitrarySizedS 32

instance Arbitrary HDAddressPayload where
    arbitrary = genericArbitrary
