{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | 'Arbitrary' unsafe instances for some types from Txp types

module Test.Pos.Chain.Txp.Arbitrary.Unsafe () where

import           Universum

import           Pos.Chain.Txp (TxOut (..))

import           Test.Pos.Core.Arbitrary.Unsafe ()
import           Test.Pos.Util.QuickCheck.Arbitrary (ArbitraryUnsafe (..))

instance ArbitraryUnsafe TxOut where
    arbitraryUnsafe = TxOut <$> arbitraryUnsafe <*> arbitraryUnsafe
