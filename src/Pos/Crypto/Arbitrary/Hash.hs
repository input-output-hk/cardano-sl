{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

-- | `Arbitrary` instances for `AbstractHash`.
--
-- Moved to a separate module to suppress `redundant constraint`
-- warning about `Binary a` constraint
-- (discussion: https://github.com/input-output-hk/pos-haskell-prototype/commit/b0655df210ffcdb3bad6610fe8af8d8d6bd4dfca#commitcomment-19576619)

module Pos.Crypto.Arbitrary.Hash () where

import           Data.Binary               (Binary)
import           Test.QuickCheck           (Arbitrary (..))
import           Test.QuickCheck.Instances ()
import           Universum

import           Pos.Crypto.Hashing        (AbstractHash, HashAlgorithm,
                                            unsafeAbstractHash)
import           Pos.Util.Arbitrary        (ArbitraryUnsafe)

instance (HashAlgorithm algo, Binary a) =>
         Arbitrary (AbstractHash algo a) where
    arbitrary = unsafeAbstractHash <$> (arbitrary @Int64)

instance (HashAlgorithm algo, Binary a) => ArbitraryUnsafe (AbstractHash algo a)
