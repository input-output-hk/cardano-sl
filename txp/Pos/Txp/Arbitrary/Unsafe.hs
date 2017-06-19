-- | 'Arbitrary' unsafe instances for some types from Txp types

module Pos.Txp.Arbitrary.Unsafe () where

import           Universum

import           Pos.Core.Types.Arbitrary.Unsafe ()
import           Pos.Txp.Core.Types              (TxOut (..))
import           Pos.Util.Arbitrary              (ArbitraryUnsafe (..))

instance ArbitraryUnsafe TxOut where
    arbitraryUnsafe = TxOut <$> arbitraryUnsafe <*> arbitraryUnsafe
