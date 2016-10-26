{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Common things used in `Pos.Crypto.Arbitrary` and `Pos.Util.Arbitrary`

module Pos.Util.Arbitrary
  ( Nonrepeating (..)
  , sublistN
  , unsafeMakePool
  ) where

import           System.IO.Unsafe (unsafePerformIO)
import           Test.QuickCheck  (Gen, shuffle)
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
