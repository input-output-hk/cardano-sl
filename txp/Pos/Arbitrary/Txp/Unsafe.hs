{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | 'Arbitrary' unsafe instances for some types from Txp types

module Pos.Arbitrary.Txp.Unsafe () where

import           Universum

import           Pos.Arbitrary.Core.Unsafe ()
import           Pos.Core.Txp (TxOut (..))

import           Test.Pos.Util.QuickCheck.Arbitrary (ArbitraryUnsafe (..))

instance ArbitraryUnsafe TxOut where
    arbitraryUnsafe = TxOut <$> arbitraryUnsafe <*> arbitraryUnsafe
