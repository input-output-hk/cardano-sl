{-# LANGUAGE StandaloneDeriving #-}

module Pos.Types.Arbitrary.Unsafe () where

import           Pos.Crypto.Arbitrary.Unsafe ()
import           Pos.Types.Types             (Address (..), Coin (..), EpochIndex (..),
                                              FtsSeed (..), LocalSlotIndex (..),
                                              SlotId (..), TxOut (..))
import           Pos.Util.Arbitrary          (ArbitraryUnsafe (..))
import           Universum

deriving instance ArbitraryUnsafe Coin
deriving instance ArbitraryUnsafe Address
deriving instance ArbitraryUnsafe FtsSeed
deriving instance ArbitraryUnsafe EpochIndex
deriving instance ArbitraryUnsafe LocalSlotIndex

instance ArbitraryUnsafe SlotId where
    arbitraryUnsafe = SlotId <$> arbitraryUnsafe <*> arbitraryUnsafe

instance ArbitraryUnsafe TxOut where
    arbitraryUnsafe = TxOut <$> arbitraryUnsafe <*> arbitraryUnsafe
