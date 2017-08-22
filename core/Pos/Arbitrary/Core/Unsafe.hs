-- | 'Arbitrary' unsafe instances for some types from 'Pos.Core.Types'.

module Pos.Arbitrary.Core.Unsafe () where

import           Universum

import           Pos.Arbitrary.Core          ()
import           Pos.Arbitrary.Crypto.Unsafe ()
import           Pos.Binary.Crypto           ()
import           Pos.Core                    (AddrAttributes (..),
                                              AddrStakeDistribution (..), AddrType (..),
                                              Address (..), Coin, EpochIndex (..),
                                              HasCoreConstants, LocalSlotIndex,
                                              SharedSeed (..), SlotId (..), mkCoin)
import           Pos.Data.Attributes         (mkAttributes)
import           Pos.Util.Arbitrary          (ArbitraryUnsafe (..))

deriving instance ArbitraryUnsafe SharedSeed
deriving instance ArbitraryUnsafe EpochIndex

instance HasCoreConstants => ArbitraryUnsafe LocalSlotIndex where

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

instance HasCoreConstants => ArbitraryUnsafe SlotId where
    arbitraryUnsafe = SlotId <$> arbitraryUnsafe <*> arbitraryUnsafe
