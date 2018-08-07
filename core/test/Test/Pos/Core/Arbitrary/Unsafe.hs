{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | 'Arbitrary' unsafe instances for some types from 'Pos.Core.Types'.

module Test.Pos.Core.Arbitrary.Unsafe () where

import           Universum

import           Pos.Core (AddrAttributes (..), AddrStakeDistribution (..),
                     AddrType (..), Address (..), Coin, EpochIndex (..),
                     LocalSlotIndex, SharedSeed (..), SlotId (..), mkCoin)
import           Pos.Core.Attributes (mkAttributes)
import           Pos.Core.Configuration (HasProtocolConstants)

import           Test.Pos.Core.Arbitrary ()
import           Test.Pos.Crypto.Arbitrary ()
import           Test.Pos.Util.QuickCheck.Arbitrary (ArbitraryUnsafe (..))

deriving instance ArbitraryUnsafe SharedSeed
deriving instance ArbitraryUnsafe EpochIndex

instance HasProtocolConstants => ArbitraryUnsafe LocalSlotIndex where

instance ArbitraryUnsafe Coin where
    arbitraryUnsafe = mkCoin <$> arbitraryUnsafe

instance ArbitraryUnsafe Address where
    arbitraryUnsafe = do
        addrRoot <- arbitraryUnsafe
        let addrAttributes =
                mkAttributes $
                AddrAttributes
                { aaPkDerivationPath = Nothing
                , aaStakeDistribution = BootstrapEraDistr
                }
        let addrType = ATPubKey
        return Address {..}

instance HasProtocolConstants => ArbitraryUnsafe SlotId where
    arbitraryUnsafe = SlotId <$> arbitraryUnsafe <*> arbitraryUnsafe
