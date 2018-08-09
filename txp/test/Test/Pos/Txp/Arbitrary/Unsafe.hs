{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | 'Arbitrary' unsafe instances for some types from Txp types

module Test.Pos.Txp.Arbitrary.Unsafe () where

import           Universum

import           Pos.Core.Txp (TxOut (..))

import           Test.Pos.Core.Arbitrary.Unsafe ()
import           Test.Pos.Util.QuickCheck.Arbitrary (ArbitraryUnsafe (..))

instance ArbitraryUnsafe TxOut where
    arbitraryUnsafe = TxOut <$> arbitraryUnsafe <*> arbitraryUnsafe
