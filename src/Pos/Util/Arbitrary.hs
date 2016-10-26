{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Common things used in `Pos.Crypto.Arbitrary` and `Pos.Util.Arbitrary`

module Pos.Util.Arbitrary
    ( Nonrepeating (..)
    , ArbitraryUnsafe (..)
    , sublistN
    , unsafeMakePool
    , arbitrarySizedS
    ) where

import           Data.ByteString  (pack)
import           System.IO.Unsafe (unsafePerformIO)
import           Test.QuickCheck  (Arbitrary (..), Gen, shuffle, vector, listOf)
import           Universum

-- | Choose a random (shuffled) subset of length n. Throws an error if
-- there's not enough elements.
sublistN :: Int -> [a] -> Gen [a]
sublistN n xs
    | length xs < n = panic "sublistN: not enough elements"
    | otherwise     = take n <$> shuffle xs

class Nonrepeating a where
    nonrepeating :: Int -> Gen [a]

-- | Unsafely create pool of `n` random values to be picked
-- (see note in `Pos.Crypto.Arbitrary` for explanation)
unsafeMakePool :: Text -> Int -> IO a -> [a]
unsafeMakePool msg n action = unsafePerformIO $ do
    putText msg
    replicateM n action

-- | Make arbitrary `ByteString` of given length
arbitrarySizedS :: StringConv ByteString s => Int -> Gen s
arbitrarySizedS n = toS . pack <$> vector n

{- ArbitraryUnsafe class
~~~~~~~~~~~~~~~~~~~~~~~~

This class is the same as `Arbitrary`, except instances of this class for
stuff like public/secret keys, VSS shares, commitments etc. are designed
not to mimic real data as presisely as possible (using OpenSSL random), but
rather to be simple and efficient.

This is especially useful for benchmarking.
-}

class ArbitraryUnsafe a where
    arbitraryUnsafe :: Gen a

instance ArbitraryUnsafe Word16 where
    arbitraryUnsafe = arbitrary

instance ArbitraryUnsafe Word32 where
    arbitraryUnsafe = arbitrary

instance ArbitraryUnsafe Word64 where
    arbitraryUnsafe = arbitrary

instance ArbitraryUnsafe ByteString where
    arbitraryUnsafe = pack <$> arbitrary

instance ArbitraryUnsafe a => ArbitraryUnsafe [a] where
    arbitraryUnsafe = listOf arbitraryUnsafe

instance (ArbitraryUnsafe a, ArbitraryUnsafe b) => ArbitraryUnsafe (a, b) where
    arbitraryUnsafe = (,) <$> arbitraryUnsafe <*> arbitraryUnsafe
