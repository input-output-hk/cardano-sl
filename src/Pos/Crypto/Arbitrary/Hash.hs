{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

-- | `Arbitrary` instances for `AbstractHash`.
--
-- Moved to a separate module to suppress `redundant constraint`
-- warning about `Binary a` constraint
-- (discussion: https://github.com/input-output-hk/pos-haskell-prototype/commit/b0655df210ffcdb3bad6610fe8af8d8d6bd4dfca#commitcomment-19576619)
-- ^ Will be fixed hopefully by moving everything to `Bi`

module Pos.Crypto.Arbitrary.Hash () where

import           Test.QuickCheck           (Arbitrary (..), choose)
import           Test.QuickCheck.Instances ()
import           Universum

import           Pos.Binary.Class          (Bi)
import           Pos.Crypto.Hashing        (AbstractHash, HashAlgorithm,
                                            unsafeAbstractHash)
import           Pos.Util.Arbitrary        (ArbitraryUnsafe)

instance (HashAlgorithm algo, Bi a) =>
         Arbitrary (AbstractHash algo a) where
    arbitrary = unsafeAbstractHash <$> choose (minBound, maxBound :: Word64)

instance (HashAlgorithm algo, Bi a) =>
         ArbitraryUnsafe (AbstractHash algo a)
