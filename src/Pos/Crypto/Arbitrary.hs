{-# LANGUAGE TypeApplications #-}
-- | `Arbitrary` instances for using in tests and benchmarks

module Pos.Crypto.Arbitrary
    ( KeyPair(..)
    ) where

import           Control.Lens                 (view, _1, _2, _3, _4)
import           Data.Binary                  (Binary)
import           Data.List.NonEmpty           (fromList)
import           System.IO.Unsafe             (unsafePerformIO)
import           Test.QuickCheck              (Arbitrary (..), choose, elements,
                                               generate)
import           Universum

import           Pos.Crypto.Arbitrary.Hash    ()
import           Pos.Crypto.Arbitrary.Unsafe  ()
import           Pos.Crypto.SecretSharing     (EncShare,Secret, SecretProof,
                                               SecretSharingExtra, Share, VssKeyPair,
                                               VssPublicKey, decryptShare,
                                               genSharedSecret, toVssPublicKey, vssKeyGen)
import           Pos.Crypto.SerTypes          (LEncShare, LSecret, LSecretProof,
                                               LSecretSharingExtra, LShare, LVssPublicKey)
import           Pos.Crypto.Signing           (PublicKey, SecretKey, Signature, Signed,
                                               keyGen, mkSigned, sign)
import           Pos.Util                     (Serialized (..))
import           Pos.Util.Arbitrary           (Nonrepeating (..), sublistN, unsafeMakePool)

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
-- Arbitrary signing keys
----------------------------------------------------------------------------

-- | 'PublicKey' with corresponding 'SecretKey'.
data KeyPair = KeyPair
    { getPub :: PublicKey
    , getSec :: SecretKey
    } deriving (Eq, Ord, Show)

keys :: [KeyPair]
keys = unsafeMakePool "[generating keys for tests...]" 50 $ uncurry KeyPair <$> keyGen
{-# NOINLINE keys #-}

instance Arbitrary KeyPair where
    arbitrary = elements keys

instance Arbitrary PublicKey where
    arbitrary = getPub <$> arbitrary
instance Arbitrary SecretKey where
    arbitrary = getSec <$> arbitrary

instance Nonrepeating KeyPair where
    nonrepeating n = sublistN n keys

instance Nonrepeating PublicKey where
    nonrepeating n = map getPub <$> nonrepeating n
instance Nonrepeating SecretKey where
    nonrepeating n = map getSec <$> nonrepeating n

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

instance Arbitrary LVssPublicKey where
    arbitrary = serialize @VssPublicKey <$> arbitrary

instance Nonrepeating VssKeyPair where
    nonrepeating n = sublistN n vssKeys

instance Nonrepeating VssPublicKey where
    nonrepeating n = map toVssPublicKey <$> nonrepeating n

----------------------------------------------------------------------------
-- Arbitrary signatures
----------------------------------------------------------------------------

instance (Binary a, Arbitrary a) => Arbitrary (Signature a) where
    arbitrary = sign <$> arbitrary <*> arbitrary

instance (Binary a, Arbitrary a) => Arbitrary (Signed a) where
    arbitrary = mkSigned <$> arbitrary <*> arbitrary

----------------------------------------------------------------------------
-- Arbitrary secrets
----------------------------------------------------------------------------

sharedSecrets :: [(SecretSharingExtra, Secret, SecretProof, [EncShare])]
sharedSecrets =
    unsafeMakePool "[generating shared secrets for tests...]" 50 $ do
        parties <- generate $ choose (1, length vssKeys)
        threshold <- generate $ choose (1, toInteger parties)
        vssKs <- generate $ sublistN parties vssKeys
        genSharedSecret threshold (map toVssPublicKey $ fromList vssKs)
{-# NOINLINE sharedSecrets #-}

instance Arbitrary SecretSharingExtra where
    arbitrary = elements . fmap (view _1) $ sharedSecrets

instance Arbitrary LSecretSharingExtra where
    arbitrary = serialize @SecretSharingExtra <$> arbitrary

instance Arbitrary LSecretProof where
    arbitrary = serialize @SecretProof <$> arbitrary

instance Arbitrary Secret where
    arbitrary = elements . fmap (view _2) $ sharedSecrets

instance Arbitrary LSecret where
    arbitrary = serialize @Secret <$> arbitrary

instance Arbitrary SecretProof where
    arbitrary = elements . fmap (view _3) $ sharedSecrets

instance Arbitrary EncShare where
    arbitrary = elements . concat . fmap (view _4) $ sharedSecrets

instance Arbitrary LEncShare where
    arbitrary = serialize @EncShare <$> arbitrary

instance Arbitrary Share where
    arbitrary = unsafePerformIO <$> (decryptShare <$> arbitrary <*> arbitrary)

instance Arbitrary LShare where
    arbitrary = serialize @Share <$> arbitrary
