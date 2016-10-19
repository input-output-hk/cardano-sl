{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}

module Test.Pos.Util
       ( Nonrepeating(..)
       , sublistN

       , KeyPair(..)
       ) where

import           System.IO.Unsafe (unsafePerformIO)
import           System.Random    (Random)
import           Test.QuickCheck  (Arbitrary (..), Gen, choose, elements, shuffle)
import           Universum

import           Pos.Constants    (epochSlots)
import           Pos.Crypto       (PublicKey, SecretKey, VssKeyPair, VssPublicKey, keyGen,
                                   toVssPublicKey, vssKeyGen)
import           Pos.Types        (Coin (..), Commitment, EpochIndex (EpochIndex),
                                   FtsSeed, LocalSlotIndex (LocalSlotIndex), Opening,
                                   SlotId (SlotId), genCommitmentAndOpening)

{- A note on 'Arbitrary' instances
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We can't make an 'Arbitrary' instance for keys or seeds because generating
them safely requires randomness which must come from IO (we could use an
'arbitrary' randomness generator for an 'Arbitrary' instance, but then what's
the point of testing key generation when we use different generators in
production and in tests?). So, we just generate lots of keys and seeds with
'unsafePerformIO' and use them for everything.
-}

-- | Choose a random (shuffled) subset of length n. Throws an error if
-- there's not enough elements.
sublistN :: Int -> [a] -> Gen [a]
sublistN n xs
    | length xs < n = panic "sublistN: not enough elements"
    | otherwise     = take n <$> shuffle xs

class Nonrepeating a where
    nonrepeating :: Int -> Gen [a]

----------------------------------------------------------------------------
-- Commitments and openings
----------------------------------------------------------------------------

commitmentsAndOpenings :: [(Commitment, Opening)]
commitmentsAndOpenings = unsafePerformIO $ do
    putText "[generating Commitments and Openings for tests...]"
    replicateM 50 $ genCommitmentAndOpening undefined undefined
{-# NOINLINE commitmentsAndOpenings #-}

instance Arbitrary (Commitment, Opening) where
    arbitrary = elements commitmentsAndOpenings

instance Nonrepeating (Commitment, Opening) where
    nonrepeating n = sublistN n commitmentsAndOpenings

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
    nonrepeating n = sublistN n keys

instance Nonrepeating PublicKey where
    nonrepeating n = map getPub <$> nonrepeating n
instance Nonrepeating SecretKey where
    nonrepeating n = map getSec <$> nonrepeating n

----------------------------------------------------------------------------
-- Arbitrary VSS keys
----------------------------------------------------------------------------

vssKeys :: [VssKeyPair]
vssKeys = unsafePerformIO $ do
    putText "[generating VSS keys for tests...]"
    replicateM 50 vssKeyGen
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
-- Arbitrary core types
----------------------------------------------------------------------------

deriving instance Arbitrary Coin

maxReasonableEpoch :: Integral a => a
maxReasonableEpoch = 5 * 1000 * 1000 * 1000 * 1000  -- 5 * 10^12, because why not

deriving instance Random EpochIndex

instance Arbitrary EpochIndex where
    arbitrary = choose (0, maxReasonableEpoch)

deriving instance Random LocalSlotIndex

instance Arbitrary LocalSlotIndex where
    arbitrary = choose (0, epochSlots - 1)

instance Arbitrary SlotId where
    arbitrary = SlotId <$> arbitrary <*> arbitrary
