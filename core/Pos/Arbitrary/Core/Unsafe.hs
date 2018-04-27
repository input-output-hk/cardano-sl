-- | 'Arbitrary' unsafe instances for some types from 'Pos.Core.Types'.

module Pos.Arbitrary.Core.Unsafe () where

import           Universum

import           Pos.Arbitrary.Core ()
import           Pos.Binary.Crypto ()
import           Pos.Core (AddrAttributes (..), AddrStakeDistribution (..), AddrType (..),
                           Address (..), Coin, EpochIndex (..), LocalSlotIndex, SharedSeed (..),
                           SlotId (..), mkCoin)
import           Pos.Core.Configuration.Protocol (HasProtocolConstants)
import           Pos.Data.Attributes (mkAttributes)
import           Pos.Util.QuickCheck.Arbitrary (ArbitraryUnsafe (..))

import           Test.Pos.Crypto.Arbitrary ()

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
