{-# LANGUAGE TypeApplications #-}

-- | `Arbitrary` instances for using in tests and benchmarks

module Pos.Crypto.Arbitrary
    ( KeyPair(..)
    ) where

import           Control.Lens                (view, _2, _4)
import           Data.Binary                 (Binary)
import           Test.QuickCheck             (Arbitrary (..), elements)
import           Universum

import           Pos.Crypto.Arbitrary.Hash   ()
import           Pos.Crypto.Arbitrary.Unsafe ()
import           Pos.Crypto.SecretSharing    (EncShare, Secret, VssKeyPair, VssPublicKey,
                                              genSharedSecret, toVssPublicKey, vssKeyGen)
import           Pos.Crypto.Signing          (PublicKey, SecretKey, Signature, Signed,
                                              keyGen, mkSigned, sign)
import           Pos.Util.Arbitrary          (Nonrepeating (..), sublistN, unsafeMakeList,
                                              unsafeMakePool)

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

secrets :: [Secret]
secrets =
    unsafeMakePool "[generating shares for tests...]" 50 $
        view _2 <$> genSharedSecret 1000 (map toVssPublicKey vssKeys)

instance Arbitrary Secret where
    arbitrary = elements secrets

encShares :: [EncShare]
encShares =
    unsafeMakeList "[generating shares for tests...]" $
        view _4 <$> genSharedSecret 1000 (map toVssPublicKey vssKeys)
{-# NOINLINE encShares #-}

instance Arbitrary EncShare where
    arbitrary = elements encShares
