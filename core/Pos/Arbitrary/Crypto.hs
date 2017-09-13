-- | `Arbitrary` instances for using in tests and benchmarks

module Pos.Arbitrary.Crypto
       ( SharedSecrets (..)
       ) where

import           Universum

import           Control.Monad                     (zipWithM)
import qualified Data.ByteArray                    as ByteArray
import           Data.List.NonEmpty                (fromList)
import           System.IO.Unsafe                  (unsafePerformIO)
import           Test.QuickCheck                   (Arbitrary (..), choose, elements,
                                                    generate, oneof, vector)
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary, genericShrink)

import           Pos.Arbitrary.Crypto.Unsafe       ()
import           Pos.Binary.Class                  (AsBinary (..), AsBinaryClass (..), Bi)
import           Pos.Binary.Crypto                 ()
import           Pos.Core.Configuration.Protocol (HasProtocolConstants)
import           Pos.Crypto.AsBinary               ()
import           Pos.Crypto.Hashing                (AbstractHash, HashAlgorithm)
import           Pos.Crypto.HD                     (HDAddressPayload, HDPassphrase (..))
import           Pos.Crypto.SecretSharing          (DecShare, EncShare, Secret,
                                                    SecretProof, Threshold, VssKeyPair,
                                                    VssPublicKey, decryptShare,
                                                    genSharedSecret, toVssPublicKey,
                                                    vssKeyGen)
import           Pos.Crypto.Signing                (ProxyCert, ProxySecretKey,
                                                    ProxySignature, PublicKey, SecretKey,
                                                    Signature, Signed, keyGen, mkSigned,
                                                    proxySign, sign, toPublic)
import           Pos.Crypto.Signing.Redeem         (RedeemPublicKey, RedeemSecretKey,
                                                    RedeemSignature, redeemKeyGen,
                                                    redeemSign)
import           Pos.Crypto.Signing.Safe           (PassPhrase, createProxyCert,
                                                    createPsk)
import           Pos.Crypto.Signing.Types.Tag      (SignTag (..))
import           Pos.Util.Arbitrary                (Nonrepeating (..), arbitraryUnsafe,
                                                    sublistN, unsafeMakePool)

{- A note on 'Arbitrary' instances
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We can't make an 'Arbitrary' instance for keys or seeds because generating
them safely requires randomness which must come from IO (we could use an
'arbitrary' randomness generator for an 'Arbitrary' instance, but then what's
the point of testing key generation when we use different generators in
production and in tests?). So, we just generate lots of keys and seeds with
'unsafePerformIO' and use them for everything.
-}

----------------------------------------------------------------------------
-- SignTag
----------------------------------------------------------------------------

instance Arbitrary SignTag where
    arbitrary = genericArbitrary
    shrink = genericShrink

----------------------------------------------------------------------------
-- Arbitrary signing keys
----------------------------------------------------------------------------

-- If you want an arbitrary keypair, just generate a secret key with
-- 'arbitrary' and then use 'Pos.Crypto.toPublic' to get the corresponding
-- public key.

keys :: [(PublicKey, SecretKey)]
keys = unsafeMakePool "[generating keys for tests...]" 50 keyGen
{-# NOINLINE keys #-}

instance Arbitrary PublicKey where
    arbitrary = fst <$> elements keys
instance Arbitrary SecretKey where
    arbitrary = snd <$> elements keys

instance Nonrepeating PublicKey where
    nonrepeating n = map fst <$> sublistN n keys
instance Nonrepeating SecretKey where
    nonrepeating n = map snd <$> sublistN n keys

-- Repeat the same for ADA redemption keys
redemptionKeys :: [(RedeemPublicKey, RedeemSecretKey)]
redemptionKeys = unsafeMakePool "[generating redemption keys for tests..]" 50 redeemKeyGen

instance Arbitrary RedeemPublicKey where
    arbitrary = fst <$> elements redemptionKeys
instance Arbitrary RedeemSecretKey where
    arbitrary = snd <$> elements redemptionKeys

instance Nonrepeating RedeemPublicKey where
    nonrepeating n = map fst <$> sublistN n redemptionKeys
instance Nonrepeating RedeemSecretKey where
    nonrepeating n = map snd <$> sublistN n redemptionKeys

----------------------------------------------------------------------------
-- Arbitrary VSS keys
----------------------------------------------------------------------------

vssKeys :: [VssKeyPair]
vssKeys = unsafeMakePool "[generating VSS keys for tests...]" 50 vssKeyGen
{-# NOINLINE vssKeys #-}

instance Arbitrary VssKeyPair where
    arbitrary = elements vssKeys

instance Arbitrary VssPublicKey where
    arbitrary = toVssPublicKey <$> arbitrary

instance Arbitrary (AsBinary VssPublicKey) where
    arbitrary = asBinary @VssPublicKey <$> arbitrary

instance Nonrepeating VssKeyPair where
    nonrepeating n = sublistN n vssKeys

instance Nonrepeating VssPublicKey where
    nonrepeating n = map toVssPublicKey <$> nonrepeating n

----------------------------------------------------------------------------
-- Arbitrary signatures
----------------------------------------------------------------------------

instance (HasProtocolConstants, Bi a, Arbitrary a) => Arbitrary (Signature a) where
    arbitrary = sign <$> arbitrary <*> arbitrary <*> arbitrary

instance (HasProtocolConstants, Bi a, Arbitrary a) => Arbitrary (RedeemSignature a) where
    arbitrary = redeemSign <$> arbitrary <*> arbitrary <*> arbitrary

instance (HasProtocolConstants, Bi a, Arbitrary a) => Arbitrary (Signed a) where
    arbitrary = mkSigned <$> arbitrary <*> arbitrary <*> arbitrary

instance (HasProtocolConstants, Bi w, Arbitrary w) => Arbitrary (ProxyCert w) where
    arbitrary = liftA3 createProxyCert arbitrary arbitrary arbitrary

instance (HasProtocolConstants, Bi w, Arbitrary w) => Arbitrary (ProxySecretKey w) where
    arbitrary = liftA3 createPsk arbitrary arbitrary arbitrary

instance (HasProtocolConstants, Bi w, Arbitrary w, Bi a, Arbitrary a) =>
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
    { ssSecret    :: Secret
    , ssSecProof  :: SecretProof
    , ssShares    :: [(EncShare, DecShare)]
    , ssThreshold :: Threshold
    , ssVSSPKs    :: [VssPublicKey]
    , ssPos       :: Int            -- This field is a valid, zero-based index in the
                                    -- shares/keys lists.
    } deriving (Show, Eq)

sharedSecrets :: [SharedSecrets]
sharedSecrets =
    unsafeMakePool "[generating shared secrets for tests...]" 50 $ do
        parties <- generate $ choose (4, length vssKeys)
        threshold <- generate $ choose (2, toInteger parties - 2)
        vssKs <- sortWith toVssPublicKey <$>
                 generate (sublistN parties vssKeys)
        (s, sp, encryptedShares) <-
            genSharedSecret threshold (map toVssPublicKey $ fromList vssKs)
        decryptedShares <- zipWithM decryptShare
                             vssKs (map snd encryptedShares)
        let shares = zip (map snd encryptedShares) decryptedShares
            vssPKs = map toVssPublicKey vssKs
        return $ SharedSecrets s sp shares threshold vssPKs (parties - 1)
{-# NOINLINE sharedSecrets #-}

instance Arbitrary Secret where
    arbitrary = elements . fmap ssSecret $ sharedSecrets

instance Arbitrary (AsBinary Secret) where
    arbitrary = asBinary @Secret <$> arbitrary

instance Arbitrary SecretProof where
    arbitrary = elements . fmap ssSecProof $ sharedSecrets

instance Arbitrary EncShare where
    arbitrary = elements . concatMap (fmap fst . ssShares) $ sharedSecrets

instance Arbitrary (AsBinary EncShare) where
    arbitrary = asBinary @EncShare <$> arbitrary

instance Arbitrary DecShare where
    arbitrary = unsafePerformIO <$> (decryptShare <$> arbitrary <*> arbitrary)

instance Arbitrary (AsBinary DecShare) where
    arbitrary = asBinary @DecShare <$> arbitrary

instance Arbitrary SharedSecrets where
    arbitrary = elements sharedSecrets

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
    arbitrary = HDPassphrase . fromString <$> vector 32

instance Arbitrary HDAddressPayload where
    arbitrary = genericArbitrary
