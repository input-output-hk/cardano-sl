-- | 'Arbitrary' unsafe instances for types from 'Pos.Txp'.

module Pos.Txp.Arbitrary.Unsafe () where

import           Universum

import           Pos.Types.Arbitrary.Core.Unsafe ()
import           Pos.Txp.Core.Types              (TxOut (..))
import           Pos.Util.Arbitrary              (ArbitraryUnsafe (..))

instance ArbitraryUnsafe TxOut where
    arbitraryUnsafe = TxOut <$> arbitraryUnsafe <*> arbitraryUnsafe
