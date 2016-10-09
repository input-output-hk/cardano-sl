module Test.Pos.Util
       ( Nonrepeating(..)

       , KeyPair(..)
       , VssKeyPair(..)
       ) where

import           System.IO.Unsafe (unsafePerformIO)
import           Test.QuickCheck  (Arbitrary (..), Gen, choose, elements, shuffle)
import           Universum

import           Pos.Constants    (epochSlots)
import           Pos.Crypto       (PublicKey, SecretKey, VssPublicKey, VssSecretKey,
                                   keyGen, vssKeyGen)
import           Pos.Types        (FtsSeed, SlotId (SlotId), genFtsSeed)

{- A note on 'Arbitrary' instances
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We can't make an 'Arbitrary' instance for keys or seeds because generating
them safely requires randomness which must come from IO (we could use an
'arbitrary' randomness generator for an 'Arbitrary' instance, but then what's
the point of testing key generation when we use different generators in
production and in tests?). So, we just generate lots of keys and seeds with
'unsafePerformIO' and use them for everything.
-}

class Nonrepeating a where
    nonrepeating :: Int -> Gen [a]

----------------------------------------------------------------------------
-- Seed
----------------------------------------------------------------------------

ftsSeeds :: [FtsSeed]
ftsSeeds = unsafePerformIO $ do
    putText "[generating FtsSeeds for tests...]"
    replicateM 50 genFtsSeed
{-# NOINLINE ftsSeeds #-}

instance Arbitrary FtsSeed where
    arbitrary = elements ftsSeeds

instance Nonrepeating FtsSeed where
    nonrepeating n
        | n > length keys = panic "nonrepeating: not enough seeds"
        | otherwise = take n <$> shuffle ftsSeeds

----------------------------------------------------------------------------
-- Arbitrary signing keys
----------------------------------------------------------------------------

data KeyPair = KeyPair
    { getPub :: PublicKey
    , getSec :: SecretKey
    } deriving (Eq, Ord, Show)

keys :: [KeyPair]
keys = unsafePerformIO $ do
    putText "[generating keys for tests...]"
    replicateM 50 (uncurry KeyPair <$> keyGen)
{-# NOINLINE keys #-}

instance Arbitrary KeyPair where
    arbitrary = elements keys

instance Arbitrary PublicKey where
    arbitrary = getPub <$> arbitrary
instance Arbitrary SecretKey where
    arbitrary = getSec <$> arbitrary

instance Nonrepeating KeyPair where
    nonrepeating n
        | n > length keys = panic "nonrepeating: not enough keys"
        | otherwise = take n <$> shuffle keys

instance Nonrepeating PublicKey where
    nonrepeating n = map getPub <$> nonrepeating n
instance Nonrepeating SecretKey where
    nonrepeating n = map getSec <$> nonrepeating n

----------------------------------------------------------------------------
-- Arbitrary VSS keys
----------------------------------------------------------------------------

data VssKeyPair = VssKeyPair
    { getVssPub :: VssPublicKey
    , getVssSec :: VssSecretKey
    } deriving (Eq, Ord, Show)

vssKeys :: [VssKeyPair]
vssKeys = unsafePerformIO $ do
    putText "[generating VSS keys for tests...]"
    replicateM 50 (uncurry VssKeyPair <$> vssKeyGen)
{-# NOINLINE vssKeys #-}

instance Arbitrary VssKeyPair where
    arbitrary = elements vssKeys

instance Arbitrary VssPublicKey where
    arbitrary = getVssPub <$> arbitrary

instance Arbitrary VssSecretKey where
    arbitrary = getVssSec <$> arbitrary

instance Nonrepeating VssKeyPair where
    nonrepeating n
        | n > length vssKeys = panic "nonrepeating: not enough keys"
        | otherwise = take n <$> shuffle vssKeys

instance Nonrepeating VssPublicKey where
    nonrepeating n = map getVssPub <$> nonrepeating n
instance Nonrepeating VssSecretKey where
    nonrepeating n = map getVssSec <$> nonrepeating n

----------------------------------------------------------------------------
-- Arbitrary core types
----------------------------------------------------------------------------

maxReasonableEpoch :: Integral a => a
maxReasonableEpoch = 5 * 1000 * 1000 * 1000 * 1000  -- 5 * 10^12, because why not

instance Arbitrary SlotId where
    arbitrary =
        SlotId <$> choose (0, maxReasonableEpoch) <*> choose (0, epochSlots - 1)
