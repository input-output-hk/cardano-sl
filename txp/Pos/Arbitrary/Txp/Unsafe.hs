-- | 'Arbitrary' unsafe instances for some types from Txp types

module Pos.Arbitrary.Txp.Unsafe () where

import           Universum

import           Pos.Arbitrary.Core.Unsafe ()
import           Pos.Txp.Core.Types        (TxOut (..))
import           Pos.Util.Arbitrary        (ArbitraryUnsafe (..))

instance ArbitraryUnsafe TxOut where
    arbitraryUnsafe = TxOut <$> arbitraryUnsafe <*> arbitraryUnsafe
