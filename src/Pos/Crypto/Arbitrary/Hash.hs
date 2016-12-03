{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

-- | `Arbitrary` instances for `Hash` and `AddressHash`
--
-- Moved to a separate module to suppress `redundant constraint`
-- warning about `Binary a` constraint
-- (discussion: https://github.com/input-output-hk/pos-haskell-prototype/commit/b0655df210ffcdb3bad6610fe8af8d8d6bd4dfca#commitcomment-19576619)

module Pos.Crypto.Arbitrary.Hash () where

import           Data.Binary               (Binary)
import           Test.QuickCheck           (Arbitrary (..), Gen)
import           Test.QuickCheck.Instances ()
import           Universum

import           Pos.Crypto.Address        (AddressHash, unsafeAddressHash)
import           Pos.Crypto.Hashing        (Hash, unsafeHash)
import           Pos.Util.Arbitrary        (ArbitraryUnsafe)

instance Binary a => Arbitrary (Hash a) where
    arbitrary = unsafeHash <$> (arbitrary :: Gen ByteString)

instance Binary a => ArbitraryUnsafe (Hash a)

instance Binary a => Arbitrary (AddressHash a) where
    arbitrary = unsafeAddressHash <$> (arbitrary :: Gen ByteString)

instance Binary a => ArbitraryUnsafe (AddressHash a)
