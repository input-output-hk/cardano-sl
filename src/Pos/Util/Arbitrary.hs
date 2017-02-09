{-# LANGUAGE UndecidableInstances #-}

-- | Common things used in `Pos.Crypto.Arbitrary` and `Pos.Util.Arbitrary`

module Pos.Util.Arbitrary
       ( Limited (..)
       , Nonrepeating (..)
       , ArbitraryUnsafe (..)
       , makeSmall
       , sublistN
       , unsafeMakeList
       , unsafeMakePool
       , arbitrarySizedS
       , arbitrarySizedSL
       , runGen
       ) where

import           Data.ByteString        (pack)
import qualified Data.ByteString.Lazy   as BL (ByteString, pack)
import           System.IO.Unsafe       (unsafePerformIO)
import           Test.QuickCheck        (Arbitrary (..), Gen, listOf, scale, shuffle,
                                         vector)
import           Test.QuickCheck.Gen    (unGen)
import           Test.QuickCheck.Random (mkQCGen)
import           Universum

makeSmall :: Gen a -> Gen a
makeSmall = scale f
  where
    f 0 = 0
    f 1 = 1
    f 2 = 2
    f 3 = 3
    f 4 = 3
    f n
      | n < 0 = n
      | otherwise =
          (round . (sqrt :: Double -> Double) . realToFrac . (`div` 3)) n


-- | Choose a random (shuffled) subset of length n. Throws an error if
-- there's not enough elements.
sublistN :: Int -> [a] -> Gen [a]
sublistN n xs
    | length xs < n = panic "sublistN: not enough elements"
    | otherwise     = take n <$> shuffle xs

-- | Type for generating list of unique (nonrepeating) elemets.
class Nonrepeating a where
    nonrepeating :: Int -> Gen [a]

-- | Unsafely create pool of `n` random values to be picked
-- (see note in `Pos.Crypto.Arbitrary` for explanation)
unsafeMakePool :: Text -> Int -> IO a -> [a]
unsafeMakePool msg n action = unsafePerformIO $ do
    putText msg
    replicateM n action

-- | Unsafely create list of `n` random values to be picked
-- (see note in `Pos.Crypto.Arbitrary` for explanation)
-- Used because genSharedSecret already returns a list
-- of EncShares, making the 'replicateM' unneeded.
unsafeMakeList :: Text -> IO [a] -> [a]
unsafeMakeList msg action = unsafePerformIO $ do
    putText msg
    action

-- | Make arbitrary `ByteString` of given length.
arbitrarySizedS :: Int -> Gen ByteString
arbitrarySizedS n = pack <$> vector n

-- | Make arbitrary `ByteString.Lazy` of given length.
arbitrarySizedSL :: Int -> Gen BL.ByteString
arbitrarySizedSL n = BL.pack <$> vector n

-- | Get something out of a quickcheck 'Gen' without having to do IO
runGen :: Gen a -> a
runGen g = unGen g (mkQCGen 31415926) 30

{-| ArbitraryUnsafe class
    ~~~~~~~~~~~~~~~~~~~~~~~~

    This class is the same as `Arbitrary`, except instances of this class for
    stuff like public/secret keys, VSS shares, commitments etc. are designed
    not to mimic real data as presisely as possible (using OpenSSL random), but
    rather to be simple and efficient.

    This is especially useful for benchmarking.

    Note: we don't need 'Generic' to derive instances of 'ArbitraryUnsafe'.
    We can either use one-line instance declaration, or @-XStandaloneDeriving@
    or @-XDeriveAnyClass@ to write something like @deriving (Arbitrary, ArbitraryUnsafe)@.
-}
class ArbitraryUnsafe a where
    arbitraryUnsafe :: Gen a

    default arbitraryUnsafe :: Arbitrary a => Gen a
    arbitraryUnsafe = arbitrary

instance ArbitraryUnsafe Word16
instance ArbitraryUnsafe Word32
instance ArbitraryUnsafe Word64
instance (Arbitrary ByteString) => ArbitraryUnsafe ByteString

instance ArbitraryUnsafe a => ArbitraryUnsafe [a] where
    arbitraryUnsafe = listOf arbitraryUnsafe

instance (ArbitraryUnsafe a, ArbitraryUnsafe b) => ArbitraryUnsafe (a, b) where
    arbitraryUnsafe = (,) <$> arbitraryUnsafe <*> arbitraryUnsafe

-- | Modifies `Arbitrary` instance to generate messages,
-- which fit into message size limit and would likely used in real launch.
newtype Limited a = Limited
    { getLimited :: a
    } deriving (Eq, Show)
