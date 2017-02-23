-- | 'Arbitrary' unsafe instances for some types from 'Pos.Types'.

module Pos.Types.Arbitrary.Unsafe () where

import           Data.Default                (def)
import           Universum

import           Pos.Binary.Crypto           ()
import           Pos.Crypto.Arbitrary.Unsafe ()
import           Pos.Data.Attributes         (mkAttributes)
import           Pos.Types.Core              (Address (..), Coin, EpochIndex (..),
                                              LocalSlotIndex (..), SlotId (..), mkCoin)
import           Pos.Types.Types             (SharedSeed (..), TxOut (..))
import           Pos.Util.Arbitrary          (ArbitraryUnsafe (..))

deriving instance ArbitraryUnsafe SharedSeed
deriving instance ArbitraryUnsafe EpochIndex
deriving instance ArbitraryUnsafe LocalSlotIndex

instance ArbitraryUnsafe Coin where
    arbitraryUnsafe = mkCoin <$> arbitraryUnsafe

instance ArbitraryUnsafe Address where
    arbitraryUnsafe = PubKeyAddress <$> arbitraryUnsafe
                                    <*> pure (mkAttributes def)

instance ArbitraryUnsafe SlotId where
    arbitraryUnsafe = SlotId <$> arbitraryUnsafe <*> arbitraryUnsafe

instance ArbitraryUnsafe TxOut where
    arbitraryUnsafe = TxOut <$> arbitraryUnsafe <*> arbitraryUnsafe
