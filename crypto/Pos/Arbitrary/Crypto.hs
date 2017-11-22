-- | `Arbitrary` instances for using in tests and benchmarks

module Pos.Arbitrary.Crypto
       ( SharedSecrets (..)
       ) where

import           Universum

import           Control.Monad (zipWithM)
import qualified Data.ByteArray as ByteArray
import           Data.List.NonEmpty (fromList)
import           Test.QuickCheck (Arbitrary (..), elements, oneof, vector)
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary, genericShrink)

import           Pos.Arbitrary.Crypto.Unsafe ()
import           Pos.Binary.Class (AsBinary (..), AsBinaryClass (..), Bi, Raw)
import           Pos.Binary.Crypto ()
import           Pos.Crypto.AsBinary ()
import           Pos.Crypto.Configuration (HasCryptoConfiguration, ProtocolMagic (..))
import           Pos.Crypto.Hashing (AHash (..), AbstractHash (..), HashAlgorithm,
                                     unsafeCheatingHashCoerce)
import           Pos.Crypto.HD (HDAddressPayload, HDPassphrase (..))
import           Pos.Crypto.Random (deterministic, randomNumberInRange)
import           Pos.Crypto.SecretSharing (DecShare, EncShare, Secret, SecretProof, Threshold,
                                           VssKeyPair, VssPublicKey, decryptShare, genSharedSecret,
                                           toVssPublicKey, vssKeyGen)
import           Pos.Crypto.Signing (ProxyCert, ProxySecretKey, ProxySignature, PublicKey,
                                     SecretKey, Signature, Signed, keyGen, mkSigned, proxySign,
                                     sign, toPublic)
import           Pos.Crypto.Signing.Redeem (RedeemPublicKey, RedeemSecretKey, RedeemSignature,
                                            redeemKeyGen, redeemSign)
import           Pos.Crypto.Signing.Safe (PassPhrase, createProxyCert, createPsk)
import           Pos.Crypto.Signing.Types.Tag (SignTag (..))
import           Pos.Util.Arbitrary (Nonrepeating (..), arbitraryUnsafe, runGen, sublistN)
import           Pos.Util.Orphans ()

deriving instance Arbitrary ProtocolMagic

{- A note on 'Arbitrary' instances
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Generating keys takes time, so we just pregenerate lots of keys in advance
and use them in 'Arbitrary' instances.
-}

keysToGenerate :: Int
keysToGenerate = 128

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
keys = deterministic "keys" $
    replicateM keysToGenerate keyGen

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
redemptionKeys = deterministic "redemptionKeys" $
    replicateM keysToGenerate redeemKeyGen

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
vssKeys = deterministic "vssKeys" $
    replicateM keysToGenerate vssKeyGen

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

instance (HasCryptoConfiguration, Bi a, Arbitrary a) => Arbitrary (Signature a) where
    arbitrary = sign <$> arbitrary <*> arbitrary <*> arbitrary

instance (HasCryptoConfiguration, Bi a, Arbitrary a) => Arbitrary (RedeemSignature a) where
    arbitrary = redeemSign <$> arbitrary <*> arbitrary <*> arbitrary

instance (HasCryptoConfiguration, Bi a, Arbitrary a) => Arbitrary (Signed a) where
    arbitrary = mkSigned <$> arbitrary <*> arbitrary <*> arbitrary

instance (HasCryptoConfiguration, Bi w, Arbitrary w) => Arbitrary (ProxyCert w) where
    arbitrary = liftA3 createProxyCert arbitrary arbitrary arbitrary

instance (HasCryptoConfiguration, Bi w, Arbitrary w) => Arbitrary (ProxySecretKey w) where
    arbitrary = liftA3 createPsk arbitrary arbitrary arbitrary

instance (HasCryptoConfiguration, Bi w, Arbitrary w, Bi a, Arbitrary a) =>
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
    { ssSecret    :: !Secret
    , ssSecProof  :: !SecretProof
    , ssEncShares :: ![EncShare]
    , ssDecShares :: ![DecShare]
    , ssThreshold :: !Threshold
    , ssVssKeys   :: ![VssPublicKey]
    , ssPos       :: !Int            -- This field is a valid, zero-based index in the
                                     -- shares/keys lists.
    } deriving (Show, Eq)

sharedSecrets :: [SharedSecrets]
sharedSecrets =
    runGen $ replicateM keysToGenerate $ do
        parties <- randomNumberInRange 4 (toInteger (length vssKeys))
        ssThreshold <- randomNumberInRange 2 (parties - 2)
        vssKs <- sortWith toVssPublicKey <$>
                 sublistN (fromInteger parties) vssKeys
        let ssVssKeys = map toVssPublicKey vssKs
        (ssSecret, ssSecProof, map snd -> ssEncShares) <-
            genSharedSecret ssThreshold (fromList ssVssKeys)
        ssDecShares <- zipWithM decryptShare vssKs ssEncShares
        let ssPos = fromInteger parties - 1
        return SharedSecrets{..}

instance Arbitrary Secret where
    arbitrary = elements . fmap ssSecret $ sharedSecrets

instance Arbitrary (AsBinary Secret) where
    arbitrary = asBinary @Secret <$> arbitrary

instance Arbitrary SecretProof where
    arbitrary = elements . fmap ssSecProof $ sharedSecrets

instance Arbitrary EncShare where
    arbitrary = elements . concatMap ssEncShares $ sharedSecrets

instance Arbitrary (AsBinary EncShare) where
    arbitrary = asBinary @EncShare <$> arbitrary

instance Arbitrary DecShare where
    arbitrary = elements . concatMap ssDecShares $ sharedSecrets

instance Arbitrary (AsBinary DecShare) where
    arbitrary = asBinary @DecShare <$> arbitrary

instance Arbitrary SharedSecrets where
    arbitrary = elements sharedSecrets

----------------------------------------------------------------------------
-- Arbitrary hashes
----------------------------------------------------------------------------

instance (HashAlgorithm algo, Bi a) => Arbitrary (AbstractHash algo a) where
    arbitrary = arbitraryUnsafe

instance Arbitrary AHash where
    arbitrary = unsafeCheatingHashCoerce @Raw <$> arbitrary

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
